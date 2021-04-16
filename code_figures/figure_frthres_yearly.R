library(openxlsx)
library(stringr)
library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
#source('loaddata.R')
#data = loaddata_h8v5(datadir)
source("assist_figure_frthres_yearly.R")
frthres = 0.7
data_year = calc_frthreshold_yearly(data,frthres)$annual
###################
source('assist_figure_multifl_cor.R')
## cor of perc / veg
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
area = read.xlsx(paste(datadir,"otherflumes_statement.xlsx",sep = "/"))
flname_sort = str_remove(area[order(area$area,decreasing = T),"flume"],"USGS 0")
flname = paste("x",str_remove(flname_sort,"USGS "),sep = "")
flname_select = flname
## daily
plt_multi_fl(get_perc_veg_cor, data, flname_select)
## yearly
plt_multi_fl(get_perc_veg_cor, data_year, flname_select, isperc = 1)
