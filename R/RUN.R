
rm(list=ls())
source("R/setup.R")
source("R/buildDataset.R")
DATASET <- buildDataset()
DT <- DATASET$DT

if(!exists("cl")) {
  CORES <- detectCores(logical = TRUE)
  cl <- makePSOCKcluster(min(CORES,40))
  registerDoParallel(cl)
}

CV.CONTROL <- trainControl(method="repeatedcv", 
                           number=10, 
                           repeats = 5,
                           allowParallel = TRUE)
PP.CONTROL <- NULL
P_SET <- c(0.70,0.75,0.8,0.85,0.9,0.95)
P <- P_SET[3]
for(P in P_SET){
  idx <- createDataPartition(DT$Na2O, p = P, list = FALSE)
  FILE <- paste0("data/Subset-P=",round(100*P,digits=0),".Rds")
  
  if(file.exists(FILE)){
    Subset <- readRDS(file=FILE)
  } else {
    Subset <- list()
    
  }
  source("R/buildSubsets.R")
  saveRDS(Subset,file=FILE)
 
  
  FILE <- paste0("data/Model-P=",round(100*P,digits=0),".Rds")
  if(file.exists(FILE)){
    Model <- readRDS(file=FILE)
  } else {
    Model <- list()
    
    
    
  }
  source("R/buildModels.R",echo=TRUE)
  saveRDS(file=FILE,Model)
}

source("R/buildSummary.R",echo=TRUE)

stopCluster(cl)
