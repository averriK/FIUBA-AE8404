buildDataset <- function(){
  
  # Lectura de Datos
  Xo <- fread("data/Vessel_X.txt")
  Yo <- fread("data/Vessel_Y.txt",select = c(1)) 
  setnames(Yo,new="Na2O")
  # Renombrar columnas para asociarlas con la frecuencia
  NEW <- sapply(colnames(Xo),function(x){
    NUM <- stringr::str_sub(x,start = 2) |> as.numeric()
    VAR <- paste0("F",NUM+99)
    return(VAR)
  },simplify = TRUE) |> as.vector()
  setnames(Xo,new=NEW)
  DT <- cbind(Xo,Yo) 
  
  
  # Construcción de los espectros
  SX <- copy(DT)
  MVARS <- colnames(Xo)
  SX[,n:=.I]
  SX <- melt(SX, id.vars = c("n","Na2O"),measure.vars = MVARS)
  SX <- SX[,.(n,Na2O,f=str_sub(variable,2,4) |> as.double(),S=value)]
  SY <- DT[,.(Na2O)]
  SY[,n:=.I]
  
  
  
  # Transformación de escalas (Box-Cox)
  
  return(list(
    DT=DT,
    SX=SX,
    SY=SY,
    Xo=Xo,
    Yo=Yo))
  
  
  # CHECK zero-Variance (caret)
  # AUX <- nearZeroVar(Xo, saveMetrics= TRUE) |> as.data.table()
  # AUX[nzv==TRUE]
  
  
}
