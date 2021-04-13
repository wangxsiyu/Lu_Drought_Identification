library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata(datadir);
###################
source('assist_figure_daybydaycor.R')
source("assist_figure_fr0_doy.R")
flname = "f001"
plt_dailycor(data,flname)
