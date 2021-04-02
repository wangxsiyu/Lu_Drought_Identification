library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata(datadir);
source("calc_annual.R")
data_year = calc_dryday(data)
###################
dataarea <- read.csv("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/old_mt/area.csv",header=T,stringsAsFactors=FALSE, sep ='\t')
area = dataarea[which(dataarea[,4] == 1),3]*0.00404686 #km^2

flname = "f001"
source('assist_figure_method2.R')
pdf(sprintf(paste(figdir,"/method2_",flname,".pdf",sep = "")), width = 16.8/2, height = 8)
plt_method2(data, data_year, flname, area)
graphics.off()

flname = "x9471000"
source('assist_figure_method2.R')
pdf(sprintf(paste(figdir,"/method2_",flname,".pdf",sep = "")), width = 16.8/2, height = 8)
plt_method2(data, data_year, flname, area)
graphics.off()
