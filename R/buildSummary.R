# *********************************************************-----
# rm(list=ls())
# source("R/setup.R")
SUM_DT <- NULL
RESAMPLES <- NULL

for(P in P_SET){
  FILE <- paste0("data/Model-P=",round(100*P,digits=0),".Rds")
  MDL <- readRDS(file=FILE)
  # str(MDL,max.level = 1)
  SUM_DT <- rbindlist(list(
    SUM_DT,
    data.table(ID="ridge",p=P,
               RMSE.train=MDL$ridge$RMSE.train,
               RMSE.test=MDL$ridge$RMSE.test,
               R2.train=MDL$ridge$R2.train,
               R2.test=MDL$ridge$R2.test,
               NVAR=nrow(MDL$ridge$VARIMP)),
    
    data.table(ID="lasso",p=P,
               RMSE.train=MDL$lasso$RMSE.train,
               RMSE.test=MDL$lasso$RMSE.test,
               R2.train=MDL$lasso$R2.train,
               R2.test=MDL$lasso$R2.test,
               NVAR=nrow(MDL$lasso$VARIMP)),
    
    data.table(ID="enet",p=P,
               RMSE.train=MDL$enet$RMSE.train,
               RMSE.test=MDL$enet$RMSE.test,
               R2.train=MDL$enet$R2.train,
               R2.test=MDL$enet$R2.test,
               NVAR=nrow(MDL$enet$VARIMP)),
    
    data.table(ID="glmnet",p=P,
               RMSE.train=MDL$glmnet$RMSE.train,
               RMSE.test=MDL$glmnet$RMSE.test,
               R2.train=MDL$glmnet$R2.train,
               R2.test=MDL$glmnet$R2.test,
               NVAR=nrow(MDL$glmnet$VARIMP)),
    
    
    
    data.table(ID="rfe_lm",p=P,
               RMSE.train=MDL$rfe_lm$RMSE.train,
               RMSE.test=MDL$rfe_lm$RMSE.test,
               R2.train=MDL$rfe_lm$R2.train,
               R2.test=MDL$rfe_lm$R2.test,
               NVAR=nrow(MDL$rfe_lm$VARIMP)),
    
    data.table(ID="pca_lm",p=P,
               RMSE.train=MDL$pca_lm$RMSE.train,
               RMSE.test=MDL$pca_lm$RMSE.test,
               R2.train=MDL$pca_lm$R2.train,
               R2.test=MDL$pca_lm$R2.test,
               NVAR=nrow(MDL$pca_lm$VARIMP)),
    
    data.table(ID="s3b_lm",p=P,
               RMSE.train=MDL$s3b_lm$RMSE.train,
               RMSE.test=MDL$s3b_lm$RMSE.test,
               R2.train=MDL$s3b_lm$R2.train,
               R2.test=MDL$s3b_lm$R2.test,
               NVAR=nrow(MDL$s3b_lm$VARIMP)),
    
    data.table(ID="pcr",p=P,
               RMSE.train=MDL$pcr$RMSE.train,
               RMSE.test=MDL$pcr$RMSE.test,
               R2.train=MDL$pcr$R2.train,
               R2.test=MDL$pcr$R2.test,
               NVAR=nrow(MDL$pcr$VARIMP)),
    
    data.table(ID="knn",p=P,
               RMSE.train=MDL$knn$RMSE.train,
               RMSE.test=MDL$knn$RMSE.test,
               R2.train=MDL$knn$R2.train,
               R2.test=MDL$knn$R2.test,
               NVAR=nrow(MDL$knn$VARIMP)),
    
    data.table(ID="rfe_knn",p=P,
               RMSE.train=MDL$rfe_knn$RMSE.train,
               RMSE.test=MDL$rfe_knn$RMSE.test,
               R2.train=MDL$rfe_knn$R2.train,
               R2.test=MDL$rfe_knn$R2.test,
               NVAR=nrow(MDL$rfe_knn$VARIMP)),
    
    data.table(ID="s3b_knn",p=P,
               RMSE.train=MDL$s3b_knn$RMSE.train,
               RMSE.test=MDL$s3b_knn$RMSE.test,
               R2.train=MDL$s3b_knn$R2.train,
               R2.test=MDL$s3b_knn$R2.test,
               NVAR=nrow(MDL$s3b_knn$VARIMP)),
    
    data.table(ID="pca_knn",p=P,
               RMSE.train=MDL$pca_knn$RMSE.train,
               RMSE.test=MDL$pca_knn$RMSE.test,
               R2.train=MDL$pca_knn$R2.train,
               R2.test=MDL$pca_knn$R2.test,
               NVAR=nrow(MDL$pca_knn$VARIMP)),
    
    data.table(ID="rf",p=P,
               RMSE.train=MDL$rf$RMSE.train,
               RMSE.test=MDL$rf$RMSE.test,
               R2.train=MDL$rf$R2.train,
               R2.test=MDL$rf$R2.test,
               NVAR=nrow(MDL$rf$VARIMP)),
    
    data.table(ID="rfe_rf",p=P,
               RMSE.train=MDL$rfe_rf$RMSE.train,
               RMSE.test=MDL$rfe_rf$RMSE.test,
               R2.train=MDL$rfe_rf$R2.train,
               R2.test=MDL$rfe_rf$R2.test,
               NVAR=nrow(MDL$rfe_rf$VARIMP)),
    
    data.table(ID="pca_rf",p=P,
               RMSE.train=MDL$pca_rf$RMSE.train,
               RMSE.test=MDL$pca_rf$RMSE.test,
               R2.train=MDL$pca_rf$R2.train,
               R2.test=MDL$pca_rf$R2.test,
               NVAR=nrow(MDL$pca_rf$VARIMP)),
    
    data.table(ID="s3b_rf",p=P,
               RMSE.train=MDL$s3b_rf$RMSE.train,
               RMSE.test=MDL$s3b_rf$RMSE.test,
               R2.train=MDL$s3b_rf$R2.train,
               R2.test=MDL$s3b_rf$R2.test,
               NVAR=nrow(MDL$s3b_rf$VARIMP))
    
    
  ))
  
}

SUM_DT[which.min(RMSE.train)]
SUM_DT[which.min(RMSE.test)]
SUM_DT[which.min(RMSE.train^2+RMSE.test^2)]
SUM_DT[which.min(1/2*(RMSE.train+RMSE.test))]

SUM_DT |> flextable() |> 
  theme_vanilla() |> fontsize(size=10) |> 
  align(align="center",part="all") |>
  align(align="left",j=2) |>
  set_table_properties(layout = "autofit") |> 
  autofit(part="all") |> 
  bold(part = "header")

fwrite(SUM_DT,"deliv/resumen.csv")
# *********************************************************-----
DATA <- SUM_DT[,.(low=min(RMSE.test)|> round(digits=3),high=max(RMSE.test) |> round(digits=3)),by=.(ID)][order(low,decreasing = TRUE)]
HC1 <- highcharter::highchart()|> 
  hc_add_series(
    DATA,# main curve
    type="columnrange",
    chartInverted=TRUE,
    hcaes(x=ID,low=low,high=high,group=ID)) |>
  hc_yAxis(
    title= list(text="RMSE (test)"),
    minorTickInterval = "auto",
    minorGridLineDashStyle = "Dot",
    showFirstLabel = FALSE,
    showLastLabel = TRUE) |>
  
  hc_xAxis(
    # title= list(text="model"),
    minorTickInterval = "auto",
    minorGridLineDashStyle = "Dot",
    showFirstLabel = TRUE,
    showLastLabel = TRUE) |>
  
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  
  hc_colors(colors = hcl.colors(n=15, palette = "ag_Sunset" ))|>
  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "vertical",
    x = 100,    y = 100
  )  |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=TRUE) |>
  
  hc_tooltip(
    sort = FALSE,
    split=FALSE,
    crosshairs = TRUE,
    pointFormat = "RMSE max={point.high}<br> RMSE min={point.low}")

HC1

# *********************************************************-----
DATA <- SUM_DT[,.(low=min(R2.test)|> round(digits=3),high=max(R2.test) |> round(digits=3)),by=.(ID)][order(low,decreasing = TRUE)]
HC3 <- highcharter::highchart()|> 
  hc_add_series(
    DATA,# main curve
    type="columnrange",
    chartInverted=TRUE,
    hcaes(x=ID,low=low,high=high,group=ID)) |>
  hc_yAxis(
    title= list(text="R2 (test)"),
    minorTickInterval = "auto",
    minorGridLineDashStyle = "Dot",
    showFirstLabel = FALSE,
    showLastLabel = TRUE) |>
  
  hc_xAxis(
    # title= list(text="model"),
    minorTickInterval = "auto",
    minorGridLineDashStyle = "Dot",
    showFirstLabel = TRUE,
    showLastLabel = TRUE) |>
  
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  
  hc_colors(colors = hcl.colors(n=15, palette = "ag_Sunset" ))|>
  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "vertical",
    x = 100,    y = 100
  )  |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=TRUE) |>
  
  hc_tooltip(
    sort = FALSE,
    split=FALSE,
    crosshairs = TRUE,
    pointFormat = "R2 max={point.high}<br> R2 min={point.low}")

HC3


# 
# DATA <- MODEL$ridge$VARIMP[,.(VI=round(VI,digits=3),ID)]
# setorderv(DATA,cols="VI",order = -1L)
# DATA |>head(25) |>flextable()

# *********************************************************-----
# FILE <- paste0("data/Model-P=95.Rds")
# MDL <- readRDS(file=FILE)
RES.RMSE <- NULL
RES.R2 <- NULL

for(P in P_SET){
  FILE <- paste0("data/Model-P=",round(100*P,digits=0),".Rds")
  MDL <- readRDS(file=FILE)
  RES <- resamples(
    list(
      ridge=MDL$ridge$CV.MDL,
      lasso=MDL$lasso$CV.MDL,
      enet=MDL$enet$CV.MDL,
      glmnet=MDL$glmnet$CV.MDL,
      pcr=MDL$pcr$CV.MDL,
      rfe_lm = MDL$rfe_lm$CV.MDL,
      pca_lm = MDL$pca_lm$CV.MDL,
      s3b_lm = MDL$s3b_lm$CV.MDL,
      knn = MDL$knn$CV.MDL,
      rfe_knn = MDL$rfe_knn$CV.MDL,
      s3b_knn = MDL$s3b_knn$CV.MDL,
      pca_knn = MDL$pca_knn$CV.MDL,
      rf=MDL$rf$CV.MDL,
      rfe_rf = MDL$rfe_rf$CV.MDL,
      s3b_rf = MDL$s3b_rf$CV.MDL,
      pca_rf = MDL$pca_rf$CV.MDL
    )
  )
  
  # box plots RMSE ----
  AUX <- summary(RES,metric=RES$metrics[2])$values |> as.data.table()
  COLS <- colnames(AUX) |> grep(pattern="RMSE",value = TRUE)
  AUX <- AUX[,..COLS]
  AUX <- melt(AUX,measure.vars = colnames(AUX))
  AUX <- AUX[,.(ID=str_remove(variable,pattern = "~RMSE"),RMSE=value)]
  RES.RMSE <- rbindlist(list(RES.RMSE,AUX))
  
  
  AUX <- summary(RES,metric=RES$metrics[3])$values |> as.data.table()
  COLS <- colnames(AUX) |> grep(pattern="Rsquared",value = TRUE)
  AUX <- AUX[,..COLS]
  AUX <- melt(AUX,measure.vars = colnames(AUX))
  AUX <- AUX[,.(ID=str_remove(variable,pattern = "~Rsquared"),RMSE=value)]
  
  RES.R2 <- rbindlist(list(RES.R2,AUX))
  
}



# *********************************************************-----
# box plots RMSE ----
BOX <- data_to_boxplot(data=RES.RMSE,variable=RMSE |> round(digits=3),group_var=ID,name="RMSE",add_outliers =FALSE)

HC8 <- highchart() |> 
  hc_xAxis(type = "category") |> 
  hc_add_series_list(BOX) |>
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=TRUE) 

HC8

# *********************************************************-----
# box plots R2 ----
BOX <- data_to_boxplot(data=RES.R2,variable=RMSE |> round(digits=3),group_var=ID,name="R2")

HC9 <- highchart() |> 
  hc_xAxis(type = "category") |> 
  hc_add_series_list(BOX) |>
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=TRUE) 
  
HC9


# trellis.par.set(caretTheme())
# dotplot(RES.R2, metric = "Rsquared")
# dotplot(RES.RMSE, metric = "RMSE")

# *********************************************************-----
# DATA <- SUM_DT[,.(low=RMSE.train|> round(digits=3),high=RMSE.train |> round(digits=3)),by=.(ID)][order(low)]
# setorderv(DATA,cols=c("ID","low"))
DATA <- SUM_DT[,.(low=min(RMSE.train)|> round(digits=3),high=max(RMSE.train) |> round(digits=3)),by=.(ID)][order(low,decreasing = TRUE)]
HC2 <- highcharter::highchart()|> 
  hc_add_series(
    DATA,# main curve
    type="columnrange",
    chartInverted=TRUE,
    hcaes(x=ID,low=low,high=high,group=ID)) |>
  hc_yAxis(
    title= list(text="RMSE (train)"),
    minorTickInterval = "auto",
    minorGridLineDashStyle = "Dot",
    showFirstLabel = FALSE,
    showLastLabel = TRUE) |>
  
  hc_xAxis(
    # title= list(text="model"),
    minorTickInterval = "auto",
    minorGridLineDashStyle = "Dot",
    showFirstLabel = TRUE,
    showLastLabel = TRUE) |>
  
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  
  hc_colors(colors = hcl.colors(n=15, palette = "Hawaii" ))|>
  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "vertical",
    x = 100,    y = 100
  )  |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=TRUE) |>
  
  hc_tooltip(
    sort = FALSE,
    split=FALSE,
    crosshairs = TRUE,
    pointFormat = "RMSE max={point.high}<br> RMSE min={point.low}")

HC2
# *********************************************************-----
DATA <- SUM_DT[,.(low=min(R2.train)|> round(digits=3),high=max(R2.train) |> round(digits=3)),by=.(ID)][order(low,decreasing = TRUE)]
HC4 <- highcharter::highchart()|> 
  hc_add_series(
    DATA,# main curve
    type="columnrange",
    chartInverted=TRUE,
    hcaes(x=ID,low=low,high=high,group=ID)) |>
  hc_yAxis(
    title= list(text="R2 (train)"),
    minorTickInterval = "auto",
    minorGridLineDashStyle = "Dot",
    showFirstLabel = FALSE,
    showLastLabel = TRUE) |>
  
  hc_xAxis(
    # title= list(text="model"),
    minorTickInterval = "auto",
    minorGridLineDashStyle = "Dot",
    showFirstLabel = TRUE,
    showLastLabel = TRUE) |>
  
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  
  hc_colors(colors = hcl.colors(n=15, palette = "Hawaii" ))|>
  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "vertical",
    x = 100,    y = 100
  )  |>
  hc_size( height = 600) |>    hc_chart(
    style=list(fontFamily = "Helvetica"),
    inverted=TRUE) |>
  
  hc_tooltip(
    sort = FALSE,
    split=FALSE,
    crosshairs = TRUE,
    pointFormat = "R2 max={point.high}<br> R2 min={point.low}")

HC4

# *********************************************************-----
DATASET <- buildDataset()
Xo <- DATASET$Xo
DATA <- cor(log(Xo))
HC10 <- hchart(DATA) |>
  hc_legend(layout = "vertical",
            align = "right",
            verticalAlign = "top") |>
  hc_colors(colors = hcl.colors(n=5, palette = "RdBu" ))|>
  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "vertical",
    x = 100,    y = 100
  )  |>
  
  hc_add_theme(hc_thm = hc_theme_hcrt()) |>
  
  # hc_size( height = 600) |>    
  hc_chart(
    style=list(fontFamily = "Helvetica"))

saveWidget(widget = HC10, file = "html/aux.html",selfcontained = TRUE)
webshot::webshot(url = "html/aux.html", file = "data/corr.png",delay = 4)

# *********************************************************-----





Results <- list()
Results$BoxPlot.RMSE <- HC8
Results$BoxPlot.R2 <- HC9
Results$Plot.RMSE.test <- HC1
Results$Plot.R2.test <- HC3
Results$Plot.RMSE.train <- HC2
Results$Plot.R2.train <- HC4
Results$Plot.Correlation <- HC10
Results$Summary <- SUM_DT
Results$Resamples <- RES
FILE <- "data/Results.Rds"
saveRDS(file=FILE,Results)
