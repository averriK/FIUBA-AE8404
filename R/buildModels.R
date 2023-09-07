DATASET <- buildDataset()
DT <- DATASET$DT
SX <- DATASET$SX
SY <- DATASET$SY


# **************************************************************** 
# 1. ridge  ----
# ****************************************************************
# ridge es insensible a la eleccion del CV
DT.train <- DT[idx,]
DT.test  <- DT[-idx,]
CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "ridge",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl =CV.CONTROL,
                tuneLength=20
)

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]
LAMBDA <- CV.MDL$finalModel$tuneValue$lambda

# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)


MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$ridge <- MDL
# **************************************************************** 
# 2. lasso  ----
# ****************************************************************
DT.train <- DT[idx,]
DT.test  <- DT[-idx,]
CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "lasso",
                # preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl =CV.CONTROL
)

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]
LAMBDA <- CV.MDL$finalModel$tuneValue$fraction


# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]


# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)

MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$lasso  <- MDL
# **************************************************************** 
# 4. enet ----
# ****************************************************************
DT.train <- DT[idx,]
DT.test  <- DT[-idx,]
CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "enet",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL,
                tuneLength = 20
)  

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]
LAMBDA <- CV.MDL$finalModel$tuneValue$lambda
ALPHA <- CV.MDL$finalModel$tuneValue$fraction

# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]


# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)


MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$enet  <- MDL

# **************************************************************** 
# 5. glmnet ----
# ****************************************************************
DT.train <- DT[idx,]
DT.test  <- DT[-idx,]
CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "glmnet",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL
)  

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]
LAMBDA <- CV.MDL$finalModel$tuneValue$lambda
ALPHA <- CV.MDL$finalModel$tuneValue$alpha

# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]


# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)


MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$glmnet  <- MDL

# **************************************************************** 
# 13. rf  ----
# ****************************************************************

DT.train <- DT[idx,]
DT.test  <- DT[-idx,]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "rf",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL,
                tuneLength=20)

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]
# RMSE <- CV.MDL$results$RMSE[I]

# RF arroja mejores resultados con 10-folds (datasets mas chicos) que con 3-folds (dataset mas grandes)

# Variable Importance
AUX <- varImp(CV.MDL,scale=TRUE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 


HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="Random Forest",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)



MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$rf  <- MDL


# **************************************************************** 
# 14. rf()+RFE  ----
# ****************************************************************
VARS <- Subset$rfe$vars
COLS <- c(VARS,"Na2O")
DT.train <- DT[idx,..COLS]
DT.test  <- DT[-idx,..COLS]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "rf",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL)

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]


# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/max(VI)]



# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)



MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$rfe_rf  <- MDL


# **************************************************************** 
# 15. rf()+PCA  ----
# ****************************************************************
DT.train <- DT[idx]
DT.test  <- DT[-idx]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "rf",
                preprocess=c("pca"),
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL)

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]

# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/max(VI)]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)

MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$pca_rf  <- MDL

# **************************************************************** 
# 16. rf()+S3B  ----
# ****************************************************************
VARS <- Subset$s3b$vars
COLS <- c(VARS,"Na2O")
DT.train <- DT[idx,..COLS]
DT.test  <- DT[-idx,..COLS]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "rf",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL)


I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]

# Variable Importance
AUX <- varImp(CV.MDL,scale=TRUE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/100]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)

MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$s3b_rf  <- MDL


# ****************************************************************
# 5. PCR  ----
# ****************************************************************
DT.train <- DT[idx,]
DT.test  <- DT[-idx,]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "pcr",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL,
                tuneGrid=expand.grid(ncomp=10:50),
                tuneLength=20
)  

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]

# Variable Importance
AUX <- varImp(CV.MDL,scale=TRUE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/max(VI)]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)


MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$pcr  <- MDL


# **************************************************************** 
# 6. lm()+RFE  ----
# ****************************************************************
VARS <- Subset$rfe$vars
COLS <- c(VARS,"Na2O")
DT.train <- DT[idx,..COLS]
DT.test  <- DT[-idx,..COLS]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "lm",
                # preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL)

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]


# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/max(VI)]



# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)




MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$rfe_lm  <- MDL


# **************************************************************** 
# 7. lm()+pca  ----
# ****************************************************************
DTP <- Subset$pca$DTP
NCP <- Subset$pca$NCP
VARS <- Subset$pca$vars[seq(1,NCP)]
COLS <- c(VARS,"Na2O")

DT.train <- DTP[idx,..COLS]
DT.test  <- DTP[-idx,..COLS]

CV.MDL <- train(Na2O ~ .,  data=DT.train,
                method = "lm",
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL
                
)  
# En el contexto de CARET, el parametro a optimizar en lm() es el intercept
I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]
# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
# VARIMP <- data.table(AUX,ID=row.names(AUX))
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_remove(row.names(AUX),pattern = "PC")))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="PC",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=ID,Y=VI)] #[X>=0.001],
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3

HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)

MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$pca_lm  <- MDL





# ****************************************************************
# 8. lm()+S3B  ----
# ****************************************************************
VARS <- Subset$s3b$vars
COLS <- c(VARS,"Na2O")
DT.train <- DT[idx,..COLS]
DT.test  <- DT[-idx,..COLS]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "lm",
                # preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL)


I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]

# Variable Importance
AUX <- varImp(CV.MDL,scale=TRUE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/100]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)

MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$s3b_lm  <- MDL



# **************************************************************** ---
# 9. knn   ----
# ****************************************************************

DT.train <- DT[idx,]
DT.test  <- DT[-idx,]
CV.MDL <- train(Na2O ~ .,  data=DT.train,
                method = "kknn",
                metric = "RMSE",
                preprocess=PP.CONTROL,
                trControl = CV.CONTROL,
                tuneLength=20
)
I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]
# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")



# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="k-nn",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)

MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$knn  <- MDL


# ****************************************************************
# 10. knn()+RFE  ----
# ****************************************************************
VARS <- Subset$rfe$vars
COLS <- c(VARS,"Na2O")
DT.train <- DT[idx,..COLS]
DT.test  <- DT[-idx,..COLS]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "kknn",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL)

I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]


# Variable Importance
AUX <- varImp(CV.MDL,scale=FALSE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/max(VI)]



# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)
MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$rfe_knn  <- MDL

# ****************************************************************
# 12. knn()+S3B  ----
# ****************************************************************
VARS <- Subset$s3b$vars
COLS <- c(VARS,"Na2O")
DT.train <- DT[idx,..COLS]
DT.test  <- DT[-idx,..COLS]

CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "kknn",
                preprocess=PP.CONTROL,
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL
)


I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]

# Variable Importance
AUX <- varImp(CV.MDL,scale=TRUE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/100]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="f[Hz]",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3
HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)


MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$s3b_knn  <- MDL

# **************************************************************** 
# 12. knn()+PCA  ----
# ****************************************************************
DT.train <- DT[idx]
DT.test  <- DT[-idx]


CV.MDL <- train(Na2O ~ ., 
                data = DT.train,  
                method = "kknn",
                preprocess=c("pca"),
                metric = "RMSE",# metric ="MAE" # metric ="Rsquared" 
                trControl = CV.CONTROL
)


I <- as.numeric(row.names(CV.MDL$bestTune))
R2 <- CV.MDL$results$Rsquared[I]

# Variable Importance
AUX <- varImp(CV.MDL,scale=TRUE)$importance
VARIMP <- data.table(AUX,ID=row.names(AUX),f=as.double(str_sub(row.names(AUX),2,4)))
setnames(VARIMP,old="Overall",new="VI")
VARIMP <- VARIMP[order(VI,decreasing = TRUE)]
VARIMP[,VI:=VI/100]

# Residuals & prediction plots
Y <- DT.train$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.train) #Predicted Values
RSS.train  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.train <- RSS.train/length(Y) #caret::MSE(Yp,Y)
RMSE.train <- sqrt(MSE.train) #caret::RMSE(Yp,Y)
TSS.train <- (Y-muY )%*%(Y-muY )|> as.double()
R2.train <-  1-RSS.train/TSS.train #caret::R2(Yp,Y)
N1 <- as.vector(idx)
DATA.train <- data.table(ID="train",Y,Yp,r=Y-Yp,n=N1 )

Y <- DT.test$Na2O #True Values
muY <- mean(Y)
Yp <- predict(CV.MDL,newdata = DT.test) #Predicted Values
RSS.test  <- (Y-Yp)%*%(Y-Yp) |> as.double()
MSE.test <- RSS.test/length(Y)
RMSE.test <- sqrt(MSE.test)
TSS.test <- (Y-muY )%*%(Y-muY )|> as.double()
R2.test <-  1-RSS.test/TSS.test #caret::R2(Yp,Y)
N2 <- seq(1,nrow(DT.train)+nrow(DT.test))[-idx]
DATA.test <- data.table(ID="test",Y,Yp,r=Y-Yp,n= N2)

DATA <- rbindlist(list(DATA.train,DATA.test)) 

#Plots 

# Plots
HC1 <- buildHCPlot(
  COLORS=c("salmon"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="PC",YT="VI",
  COLUMN=VARIMP[,.(ID="model",X=f,Y=VI)]
)
# HC1

HC2 <- buildHCPlot(
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="n",YT="r",TIP = "ID:{point.series.name}<br> n={point.x}<br> r={point.y}",
  POINT=DATA[,.(ID,X=n,Y=r)],
  CURVE=DATA[,.(ID="r==0",X=n,Y=0)]
)
# HC2

HC3 <- buildHCPlot(
  LINE="Dot",
  COLORS=c("grey","red","blue"),# hcl.colors(n=3,palette="ag_Sunset"),
  LAYOUT="horizontal",
  XLOG = FALSE,YLOG = FALSE,XREV=FALSE,YREV=FALSE,
  XT="Y",YT="Yp",
  YMAX=+5,YMIN=-5,
  POINT=DATA[,.(ID,X=Y,Y=Yp)], #[X>=0.001],
  CURVE=DATA[,.(ID="y==yp",X=Y,Y=Y)]
)
# HC3

HC4 <- highchart() |> 
  hc_add_series(data=density(DATA[ID=="train"]$r),type="area",name="train",color="blue") |> 
  hc_add_series(data=density(DATA[ID=="test"]$r),type="area",name="test",color="red") |> 
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=FALSE)

Model$pca_knn <- list()

MDL <- list()
MDL$Plot.VarImp <- HC1
MDL$Plot.Residuals <- HC2
MDL$Plot.Predictors <- HC3
MDL$Plot.Density <- HC4
MDL$MSE.train <- MSE.train
MDL$MSE.test <- MSE.test
MDL$RMSE.train <- RMSE.train
MDL$RMSE.test <- RMSE.test
MDL$VARIMP<- VARIMP
MDL$R2.train<- R2.train
MDL$R2.test<- R2.test
MDL$RSS.train<- RSS.train
MDL$RSS.test<- RSS.test
MDL$CV.MDL <- CV.MDL
MDL$DATA <- DATA
Model$pca_knn  <- MDL

# **************************************************************** 




