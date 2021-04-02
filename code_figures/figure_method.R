library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
figdir = "../../figs"
source('loaddata.R')
data_pqf = loaddata_pqf(datadir);
###################
source('assist_figure_method.R')
pdf(sprintf(paste(figdir,"1973_typical_method.pdf",sep = "/")), width = 16.8/2, height = 8)
plt_method_ill(data_pqf,flname)
graphics.off()
