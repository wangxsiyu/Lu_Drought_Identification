library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata(datadir);
source('calc_annual.R')
data_an = calc_annual(data)$annual
source("calc_residue.R")
data_residue = calc_residue(data)
###################
source('assist_figure_multifl_cor.R')
source('assist_figure_residue.R')
## daily
veg_perc_daily = plt_multi_fl(get_perc_veg_cor, data, flname, isres = 0)
## yearly
veg_perc_year = plt_multi_fl(get_perc_veg_cor, data_an, flname, isres = 0)
## residue
veg_perc_res = plt_multi_fl(plt_residue_box, data_residue, flname, isres = 1)

