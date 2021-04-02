library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata_h8v5(datadir);
source('calc_annual.R')
data_an = calc_annual(data)$annual
#source("calc_residue.R")
#data_residue = calc_residue(data)
###################
source('assist_figure_8method_comp.R')
source('assist_figure_multifl_cor.R')
percname = names(data)[str_detect(names(data),"perc")]
########## correlation of veg and 8 perc methods
##### only 1 flume
flname = "f001"
plt_8method_1fl_comp(data,data_an,flname,percname)
##### multi flume
## 1.1 no baseflow
flname_all = c("x9472050","f001","f002","f006","f015","f009","x9470700","f010","f007")
plt_8method_multifl_comp(data,flname_all,percname,"Daily")
plt_8method_multifl_comp(data_an,flname_all,percname,"Yearly")

## 1.2 has baseflow
flname_all = c("x9471380","x9470800","x9470750","x9471310",
               "x9471550","x9471000","x9470500","x9471400")
plt_8method_multifl_comp(data,flname_all,percname,"Daily")
plt_8method_multifl_comp(data_an,flname_all,percname,"Yearly")




legend("topleft", legend = toupper(rownames(daily)), col = color, lty = 1, pch = 16,
        y.intersp = 0.5, x.intersp = 0.3)






