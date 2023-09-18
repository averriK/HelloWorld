library(data.table)
library(readxl)
library(stringr)
library(flextable)
library(caret)
library(webshot)

library(htmlwidgets)
# flextable ----
knitr::opts_chunk$set("ft.shadow" = FALSE)
flextable::use_df_printer()
flextable::set_flextable_defaults( font.family="Helvetica",border.color = "gray")


# hicharter ----
library(highcharter)
library(hrbrthemes,warn.conflicts = F, quietly = T)

r <- function(x,d=0){round(x,digits=d)}


HC_THEMES <- list(
  hc_theme_smpl(),
  hc_theme_db() ,
  hc_theme_538(),
  hc_theme_alone(),
  hc_theme_bloom(),
  hc_theme_chalk(),
  hc_theme_darkunica(),
  hc_theme_economist(),
  hc_theme_elementary(),
  hc_theme_ffx(),
  hc_theme_flat(),
  hc_theme_flatdark(),
  hc_theme_ft(),
  hc_theme_ggplot2(),
  hc_theme_google(),
  hc_theme_gridlight(),
  hc_theme_handdrawn(),
  hc_theme_hcrt(),
  hc_theme_monokai(),
  hc_theme_null(),
  hc_theme_sandsignika(),
  hc_theme_superheroes(),
  hc_theme_tufte()
)

# .buildPlot ----

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


#  resave ----
resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}



# .getPGAmodel ----
# Usage:
# DATA=data.table(Sa,TR)
# .getPGAmodel(DATA)

.getPGAmodel<- function(.SD=NULL){
  DATA <- .SD[,.(LnA=log(Sa),LnTr=log(TR),AEP=1/TR)]
  MDL <- lm( LnA~., data = DATA)
  SMDL <- summary(MDL)
  a <- unname(MDL$coefficients)[1] 
  b <- unname(MDL$coefficients)[2] 
  c <- unname(MDL$coefficients)[3] 
  sd <- SMDL$sigma
  R2 <- SMDL$r.squared
  DT <- data.table(a=a,b=b,c=c,sdLnA=sd,R2=R2)
  return(DT)
}


