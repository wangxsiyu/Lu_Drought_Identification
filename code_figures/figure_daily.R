library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata(datadir);
###################
source('assist_figure_daily.R')
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
plt_flume_daily(data, "f001", percname)
#plt_flume_daily(data, "f002", percname)

################### zero in monsoon / non-monsoon
plt_monsoon_zero(data, "f001", "ndvi")
