datadir = "/Users/Mengtian/Mengtian_Project/drought_identification/data"
funcdir = "/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification"
setwd(funcdir)
source('loaddata.R')
data = loaddata(datadir);
# smooth
data$obs = preprocess(obs_d) 
# calculate annual data includes dry days
source("calc_annual.R")
data_year = calc_dryday(data)
###################
source('assist_figure_daily.R')
## 3.1 perc / veg
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
plt_flume_daily(data_year,"f001",percname)

# ## 3.2 dry days / veg
drydayname = c("dryday_t1c0f1","dryday_t0c0f0","dryday_t1c1f1")
plt_flume_daily(data_year,"f001",drydayname)
