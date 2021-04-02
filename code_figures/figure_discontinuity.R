library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
#data = loaddata(datadir)
data = loaddata_h8v5(datadir)
source('calc_annual.R')
data_year = calc_annual(data)
annual = data_year$annual
###################
source('assist_figure_discontinuity.R')

#years = c(2005,2008,2009)
years = c(2008,2010,2014)
###### 1. several specific years
flname = "f001"
plt_3year(years, data, annual, flname, 1956)

flname = "x9472050"
plt_3year(years, data, annual, flname, 1995)

###### 2. average of dry,wet,normal years
plt_drynorwet_3year(data_year, flname)



