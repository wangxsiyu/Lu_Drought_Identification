library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata(datadir);
data0 = data;

###################
source('assist_figure_daybydaycor.R')
source("assist_figure_fr0_doy.R")
flname = "x9471000"
plt_dailycor(data,flname)


getlag <- function(x, lag){
  x = filter(x, rep(1, lag)/lag, side = 1);
  return(x)
}
vegname = c("ndvi","evi","lai")
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
lag = 5;
for (pi in 1:length(percname)){
   pn = percname[pi];
   data[[pn]][, flname] = getlag(data0[[pn]][, flname], lag)
}
flname = 'f001'
plt_nacor(data, flname)
plt_selectday_scatter(data,flname,100)
