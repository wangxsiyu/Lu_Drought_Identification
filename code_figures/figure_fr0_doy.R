datadir = "/Users/Mengtian/Mengtian_Project/drought_identification/data"
funcdir = "/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification"
setwd(funcdir)
source('loaddata.R')
data = loaddata(datadir);
###################
source('assist_figure_fr0_doy.R')
td = data$obs
fr_0 = get_doy_fr_0(td)
###################
plt_doy_fr_0(fr_0,"f001")
mtext(side = 1, line = 2, text = "DOY")
mtext(side = 2, line = 2.5, text = "Zero fraction")
