datadir = "/Users/Mengtian/Mengtian_Project/drought_identification/data"
funcdir = "/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification"
setwd(funcdir)
source('loaddata.R')
data = loaddata(datadir);
# smooth
data$obs = preprocess(obs_d) 
# calculate residue
source("calc_residue.R")
data_residue = calc_residue(data)
###################
source('assist_figure_residue.R')
flname = "f001"
# boxplot
plt_residue_box(data_residue, flname)

# res>0 percentage of perc bin
plt_residue_perc(data_residue, flname)
