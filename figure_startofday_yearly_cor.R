datadir = "/Users/Mengtian/Mengtian_Project/drought_identification/data"
funcdir = "/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification"
setwd(funcdir)
source('loaddata.R')
data = loaddata(datadir);
# smooth
data$obs = preprocess(obs_d) 
source("calc_annual.R")
data_year = calc_annual(data)
###################
source('assist_figure_startofday_yearly_cor.R')

