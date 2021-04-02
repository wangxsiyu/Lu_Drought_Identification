library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
#data = loaddata(datadir);
data = loaddata_h8v5(datadir)
# calculate annual data includes dry days
source("calc_annual.R")
data_year = calc_dryday(data)
###################
source('assist_figure_yearly.R')
## 3.1 perc / veg
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
plt_flume_daily(data_year,"f001",percname)
plt_flume_daily(data_year,"x9472050",percname)

# ## 3.2 dry days / veg
drydayname = c("dryday_t1c0f1","dryday_t0c0f0","dryday_t1c1f1")
plt_flume_daily(data_year,"f001",drydayname)
plt_flume_daily(data_year,"x9472050",drydayname)
