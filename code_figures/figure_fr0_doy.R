datadir = "/Users/Mengtian/Mengtian_Project/drought_identification/data"
funcdir = "/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification"
setwd(funcdir)
source('loaddata.R')
data = loaddata(datadir);
###################
source('assist_figure_fr0_doy.R')
plt_doy_fr_0(data,"f001")
mtext(side = 1, line = 2, text = "DOY")
mtext(side = 2, line = 2.5, text = "Zero fraction")
