library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata_h8v5(datadir);
#################################
source("assist_figure_lag.R")
flname = "f001"
# lag = -7:7
# lag_cor = get_cor_lag(data, flname, lag)  
# plt_cor_lag(lag_cor,lag)


source("calc_daily_percmean.R")
recor = calc_cor_percmean(data, flname)
lag = 1:length(recor)
plt_cor_lag_siyu(recor,lag)
