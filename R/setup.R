
library(data.table) |> suppressPackageStartupMessages()
library(flextable) # Plot Tables
library(webshot)
library(highcharter)  |> suppressPackageStartupMessages()
library(glmnet) |> suppressPackageStartupMessages()
library(hrbrthemes) |> suppressPackageStartupMessages()
library(stringr)
library(caret) |> suppressPackageStartupMessages()
library(randomForest) |> suppressPackageStartupMessages()
library(RColorBrewer)
library(MASS)  # Para poder usar la funcion stepAIC
library(kknn)
library(leaps)
library(doParallel)



set.seed(2511970)
# knit options ----
knitr::opts_chunk$set(
  # fig.cap = FALSE,# esta opcion produce un FALSE al pie de cada tabla... NO USAR
  "ft.shadow" = FALSE)
# flextable  ----
use_df_printer()


buidQQplot <- function(X=NULL,ID=NULL){
  PLOT <- ggplot() +
    geom_qq(distribution = stats::qnorm,mapping =aes(sample=X, color="red"),show.legend = FALSE) + 
    geom_qq_line(distribution = stats::qnorm,mapping =aes(sample=X, color="blue"),show.legend = FALSE) + 
    theme_bw()
  FILE <- file.path("img",paste0("qq",ID,".png"))
  ggsave(plot=PLOT,filename=FILE,width = 6,height = 2, dpi = 600 )
  PLOT
  # PLOT <- ggplot() +
  #   geom_histogram(mapping = aes(x=X),bins = 60,show.legend = FALSE) + theme_bw()
  # FILE <- file.path("img",paste0("h",ID,".png"))
  # ggsave(plot=PLOT,filename=FILE,width = 6,height = 2, dpi = 600 )
  
}



buildHCPlot <- function(
    HEIGHT=600,
    LEGEND=TRUE,
    TIP = "ID:{point.series.name}<br> X={point.x}<br> Y={point.y}",
    CURVE=NULL,  BAR=NULL,  POINT=NULL,COLUMN=NULL,
    XT="X []",YT="Y []",
    COLORS=hcl.colors(n= 5,palette = "ag_Sunset",alpha=0.6),
    TYPE="line",
    XLOG=TRUE,YLOG=FALSE,
    XREV=FALSE,YREV=FALSE,
    XMAX=NULL, YMAX=NULL,
    XMIN=NULL, YMIN=NULL,
    LAYOUT="horizontal",ALIGN="right",VALIGN="top",
    LINE="Solid",
    #c('Solid','ShortDash','ShortDot','ShortDashDot','ShortDashDotDot','Dot','Dash','LongDash','DashDot','LongDashDot','LongDashDotDot')
    THEME=hc_theme_hcrt(),
    SYMBOL="circle"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  stopifnot(!is.null(POINT)|!is.null(CURVE)|!is.null(BAR)|!is.null(COLUMN))
  
  
  
  HC <- highcharter::highchart()
  
  if(!is.null(CURVE)){
    HC <- HC |> hc_add_series(
      CURVE[,.(ID,X,Y)],# main curve
      type=TYPE,
      dashStyle = LINE,
      hcaes(x=X,y=Y,group=ID)) 
    
    
  }
  if(!is.null(BAR)){
    HC <- HC |> hc_add_series(
      BAR[,.(ID,X,Y)],# main curve
      type="bar",
      hcaes(x=X,y=Y,group=ID))
    
  }
  if(!is.null(COLUMN)){
    HC <- HC |> hc_add_series(
      COLUMN[,.(ID,X,Y)],# main curve
      type="column",
      hcaes(x=X,y=Y,group=ID))
    
  }
  if(!is.null(POINT)){
    HC <- HC |>  hc_add_series(
      POINT[,.(ID,X,Y)],# main curve
      type="scatter",
      marker=list(symbol=SYMBOL),
      hcaes(x=X,y=Y,group=ID))
    
  }
  
  
  HC <- HC |>
    hc_yAxis(
      title= list(text=YT),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = FALSE,
      showLastLabel = TRUE) |>
    
    hc_xAxis(
      title= list(text=XT),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = TRUE,
      showLastLabel = TRUE) |>
    
    hc_add_theme(hc_thm = THEME) |>
    
    hc_colors(
      # colors <- c("blue","brown","red")
      # colors = hcl.colors(12,palette = PALETTE)
      colors = COLORS
    ) |>
    
    hc_tooltip(
      sort = FALSE,
      split=FALSE,
      crosshairs = TRUE,
      pointFormat = TIP) |>
    
    hc_size( height = HEIGHT) |>
    hc_legend(enabled = LEGEND)
  
  if(LEGEND==TRUE){
    
    HC <- HC |>
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = LAYOUT,
        x = 100,
        y = 100
      )  |>
      hc_chart(
        style=list(fontFamily = "Helvetica"))
  }
  
  if(!is.null(XMAX)){
    HC <- HC |> hc_xAxis(max = XMAX)
  }
  if(!is.null(YMAX)){
    HC <- HC |> hc_yAxis(max = YMAX)
  }
  
  if(!is.null(XMIN)){
    HC <- HC |> hc_xAxis(min = XMIN)
  }
  if(!is.null(YMIN)){
    HC <- HC |> hc_yAxis(min = YMIN)
  }
  if(YLOG==TRUE) {
    HC <- HC |> hc_yAxis(type = "logarithmic")
  }
  if(XLOG==TRUE) {
    HC <- HC |> hc_xAxis(reversed=XREV,type = "logarithmic")
  } else {
    HC <- HC |> hc_xAxis(reversed=XREV)
  }
  
  if(YLOG==TRUE) {
    HC <- HC |> hc_yAxis(reversed=YREV,type = "logarithmic")
  } else {
    HC <- HC |> hc_yAxis(reversed=YREV)
  }
  return(HC)
}


resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}










