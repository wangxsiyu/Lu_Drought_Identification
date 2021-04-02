library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata(datadir);
# annual data
source('calc_annual.R')
data_year = calc_annual(data)
annual = data_year$annual
###################
source('assist_figure_motivation.R')
source('assist_figure_fr0_doy.R')

flname = "f001"
pdf(sprintf(paste(figdir,"/motivation_",flname,".pdf",sep = "")), width = 16.8, height = 8)
plt_motiv(data,annual,flname)
graphics.off()



