# **************************************************************** 
# Filtros basados en splines (S3B)  ----
# ****************************************************************
# Approach B: Un subset por el promedio de todos los samples
DATASET <- buildDataset()
DT <- DATASET$DT
SX <- DATASET$SX
SY <- DATASET$SY
SX.train <- SX[n %in% idx,]
SX.test  <- SX[!(n %in% idx),]
SY.train <- SY[idx,]
SY.test  <- SY[-idx,]
VARS <- NULL
LMX <- NULL

Freq <- SX.train$f |> unique()
NFOLDS <- 10 # Numero de Folds: 9 A 54
REPEAT <- 100
k <- 1
while(k<REPEAT){
  SAMPLE <- SX.train[n %in% sample(x=n,size=round(P*180/NFOLDS)),.(f,S)] 
  MDL  <- smooth.spline(x=SAMPLE$f,y=SAMPLE$S)
  AUX <- predict(MDL)
  DATA <- data.table(f=AUX$x,S=AUX$y)
  i <-  which(diff(sign(diff(DATA$S)))==-2)+1
  LMX <- c(LMX,i) |> unique()
  
  k <- k+1
}
# Agregar primer frecuencia (es un maximo)
LMX <- c(1,LMX) |> sort(decreasing = FALSE)
# remover frecuencias contiguas
I <- !(abs(diff(LMX)) <=5)
LMX <- LMX[I]
VARS <- paste0("F", Freq[LMX])

# Plot
SAMPLE <- SX.test[,.(f,S)] # SX.train[,.(f,S)]
MDL  <- smooth.spline(x=SAMPLE$f,y=SAMPLE$S)
AUX <- predict(MDL)
DATA <- data.table(f=AUX$x,S=AUX$y)
DATA[,VAR:=paste0("F",f)]
HC0 <- buildHCPlot(
  LEGEND=TRUE,
  COLORS=hcl.colors(n=15,palette="Hawaii"),
  LAYOUT="vertical",
  XLOG = FALSE,YLOG = TRUE,XREV=FALSE,YREV=FALSE,
  XT="fn",YT="Sn ",
  CURVE=DATA[,.(ID="spline (Train)",X=f,Y=S)],
  COLUMN=DATA[LMX,.(ID=VAR,X=f,Y=S)]
)
# HC0

Subset$s3b <- list()
Subset$s3b$vars <- VARS
Subset$s3b$nvars <- length(VARS)
Subset$s3b$Plot.Spectra <- HC0


# **************************************************************** 
# Backwards Feature Selection (recursive Feature Elimination)  ----
# ****************************************************************
DATASET <- buildDataset()
DT <- DATASET$DT
DT.train <- DT[idx,]
DT.test  <- DT[-idx,]
CV.SET <- rfe(Na2O ~ ., 
              data = DT.train, 
              sizes=c(10:50),
              metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
              rfeControl =  rfeControl(
                functions=rfFuncs, 
                method="repeatedcv", 
                number=10,
                repeats=5,
                allowParallel = TRUE))

# Variables seleccionadas
VARS.RFE <- predictors(CV.SET)
# "codo" con valor minimo (36)
plot(CV.SET$results$Variables,CV.SET$results$RMSE)

Subset$rfe <- list()
Subset$rfe$vars <- predictors(CV.SET)
Subset$rfe$nvars <- CV.SET$bestSubset



# **************************************************************** 
# Principal Component Analysis  ----
# ****************************************************************
DATASET <- buildDataset()
DT <- DATASET$DT
SX <- DATASET$SX
SY <- DATASET$SY
Xo <- DT[,-c("Na2O")]
Yo <- DT$Na2O

MDL <- prcomp(Xo, scale.=TRUE,center=TRUE)
SD <- MDL$sdev
EigenValues <- SD^2
VarExp <- EigenValues/sum(EigenValues)
CumVarExp <- cumsum(VarExp)
NCP <- length(CumVarExp[CumVarExp<=0.98] )
# XP <- MDL$x[,1:NCP] |> as.data.table()
XP <- MDL$x |> as.data.table()

DTP <- cbind(XP,Na2O=Yo) |> as.data.table()

Subset$pca <- list()
Subset$pca$vars <- colnames(XP)
Subset$pca$DTP <- DTP
Subset$pca$NCP <- NCP
Subset$pca$CumVarExp <- CumVarExp
