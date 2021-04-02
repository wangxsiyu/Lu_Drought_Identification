library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
figdir = "../../figs/"
source('loaddata.R')
#data = loaddata(datadir);
data = loaddata_h8v5(datadir)
# calculate residue
source("calc_residue.R")
data_residue = calc_residue(data)
###################
source('assist_figure_residue.R')
## boxplot
#1. bar
flname = "f001"
pdf(sprintf(paste(figdir,flname,"_residue_boxplot.pdf",sep = "")), width = 16.8/2, height = 8)
stat = plt_residue_box(data_residue, flname)
graphics.off()

flname = "x9472050"
pdf(sprintf(paste(figdir,flname,"_residue_boxplot.pdf",sep = "")), width = 16.8/2, height = 8)
stat = plt_residue_box(data_residue, flname)
graphics.off()

#2.line


#stat = plt_residue_vio(data_residue, flname)

## res>0 percentage of perc bin
plt_residue_perc(data_residue, flname)
