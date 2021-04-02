library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
#data = loaddata(datadir);
data = loaddata_h8v5(datadir)
###################
source('assist_figure_startofday_yearly_cor.R')
#data_year_lag = calc_lag_annual(data)
#save(data_year_lag, file = paste(datadir,"pieter_data_year_lag.R",sep = "/"))
load(file = paste(datadir,"data_year_lag.R",sep = "/"))

flname = "x9471000" #"f001"
## veg vs perc
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
plt_veg_lag_cor(data_year_lag$annual_lag,flname,percname)

## veg vs dryday
drydname = c("dryday_t1c0f1","dryday_t0c0f0","dryday_t1c1f1")
plt_veg_lag_cor(data_year_lag$annual_lag,flname,drydname)
