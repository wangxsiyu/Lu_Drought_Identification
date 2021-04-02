library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata(datadir);
###################
source('assist_figure_heatmap.R')
flname = "f001"
plt_heatmap(data,flname)

flname = "x9472050"
plt_heatmap(data,flname)

flname = "x9471000"
plt_heatmap(data,flname)
