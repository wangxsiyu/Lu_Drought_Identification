datadir = "/Users/Mengtian/Mengtian_Project/drought_identification/data"
funcdir = "/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification"
setwd(funcdir)
source('loaddata.R')
data = loaddata(datadir);
# calculate residue
source("calc_residue.R")
data_residue = calc_residue(data)
###################
source('assist_figure_residue.R')
flname = "f001"
# boxplot
stat = plt_residue_box(data_residue, flname)

stat = plt_residue_vio(data_residue, flname)

# res>0 percentage of perc bin
plt_residue_perc(data_residue, flname)
