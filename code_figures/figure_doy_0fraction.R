td = data$obs
flname_all = setdiff(colnames(td),c("year","month","day"))
doy = get_dayid(td$month,td$day)
day365 = max(365, max(doy))
fl_doyfr = matrix(NA,day365,length(flname_all))
colnames(fl_doyfr) = flname_all
## yearly
data_an = calc_annual(data)$annual
veg_cor = get_veg_obs(get_perc_veg_cor, data_an, flname_all)$veg


get_veg_obs <- function(func, data, flname){
  vegname = c("ndvi","evi","lai")
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  varname = percname
  veg_fl = NULL
  obs = matrix(NA,length(flname),length(vegname))
  fl = matrix(list(),1,length(flname))
  for( i in 1:length(vegname) ){
    for( fli in 1:length(flname) ){
      fl[[fli]] = func(data, flname[fli], varname)
      veg_fl = rbind(veg_fl,fl[[fli]][i,])
      obs[fli,] = get_obs_veg_cor(data, flname[fli])
    }
  }
  rownames(veg_fl) = rep(vegname,each=length(flname))
  colnames(obs) = vegname
  veg_obs = list()
  veg_obs$veg = veg_fl
  veg_obs$obs = obs
  
  return(veg_obs)
}



#######  plot  ########################
col_t1c0f1 = rgb(0,0,1,0.7)   
col_t0c0f0 = rgb(1,0.1,0,0.7)  
col_t1c1f1 = rgb(0,1,0,0.7)
colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)

## 0 fraction
par(mfrow = c(3,7))
par(oma = c(2,2,1,1), mar = c(2,2,1,1))
for( fli in 1:length(flname_all) ){
  fl_doyfr[,fli] = get_doy_fr_0(td,flname_all[fli])
  plot(fl_doyfr[,fli], type = "l", col = "grey", ylim = c(0,1), las = 1, lty = 2)
  abline(h = 0.8, lty = 2)
  legend(-100,1, legend = round(veg_cor[fli,],2), text.col = colorname, 
         bty = "n", x.intersp = 0.1, y.intersp = 0.3)
  #legend(0,1, legend = round(veg_cor[fli+21,],2), text.col = colorname, 
  #       bty = "n", x.intersp = 0.1, y.intersp = 0.3)
  #legend(100,1, legend = round(veg_cor[fli+42,],2), text.col = colorname, 
  #       bty = "n", x.intersp = 0.1, y.intersp = 0.3)
  dailycor = calc_dailycor(data,flname_all[fli]) 
  lines(dailycor[[1,1]], ylim = c(0,1), col = colorname[1]) ## ndvi day by day
  lines(dailycor[[1,2]], ylim = c(0,1), col = colorname[2])
  lines(dailycor[[1,3]], ylim = c(0,1), col = colorname[3])
}














