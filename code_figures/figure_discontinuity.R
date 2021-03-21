datadir = "/Users/Mengtian/Mengtian_Project/drought_identification/data"
funcdir = "/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification"
setwd(funcdir)
source('loaddata.R')
data = loaddata(datadir);
# smooth
data$obs = preprocess(obs_d) 
###################
source('assist_figure_discontinuity.R')
flname = "f001"
idx = data$obs$year>=1958 & data$obs$year<=2017
par(mfrow = c(1,1))
par(oma = c(2,2,1,1), mar = c(1,1,1,1))
plt_discontinuty(idx, data, flname)

years = c(2001:2002,2004:2010)
par(mfrow = c(2,5))
for ( i in 1: length(years) ){
  idx = data$obs$year == years[i]
  plt_discontinuty(idx, data, flname)
  mtext(side = 3, line = 0.5, text = years[i])
}
