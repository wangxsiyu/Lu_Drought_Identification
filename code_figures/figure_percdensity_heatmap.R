library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata_h8v5(datadir);
#################################
source('assist_figure_percdensity_heatmap.R')
td = data$obs
flname_all = setdiff(colnames(td),c("year","month","day","X"))
fli = 21
{
flname = flname_all[fli]
#perci = 1
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")

par(mfrow = c(3,1))
par(oma = c(1,2,1,1), mar = c(1,2,1,1))
for( perci in 1:length(percname)){
  heatm_dens = plt_perc_hist_heatmap(data,percname[perci],flname)
}
}
