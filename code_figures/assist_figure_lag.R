get_veglag_data <- function(obs,ndvi,lagi=0){
  year = obs$year
  month = obs$month
  day = obs$day
  obs_ndvi = NULL
  nona_ndvi = ndvi[which(!is.na(ndvi[,flname])),]
  for( i in 1:nrow(nona_ndvi) ){
    idx_n = which(year == nona_ndvi$year[i] & month == nona_ndvi$month[i] & day == nona_ndvi$day[i]) + lagi
    obs_ndvi = rbind(obs_ndvi,obs[idx_n,])
  }
  return(list(obs = obs_ndvi,veg = nona_ndvi))
}



get_cor_lag <- function(data, flname, lag){
  vegname = c("ndvi","evi","lai")
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  veg_perc_cor = matrix(NA,length(vegname),length(percname))
  lag_cor = matrix(list(),1,length(lag))
  veg_obs_cor = NULL
  tobs = data$obs
  
  for( lagi in 1:length(lag) ){
    for( i in 1:length(vegname)){
      for( j in 1:length(percname)){
        tperc = data[[percname[j]]]
        tveg = data[[vegname[i]]]
        new_perc = get_veglag_data(tperc,tveg,lag[lagi])$obs
        new_obs = get_veglag_data(tobs,tveg,lag[lagi])$obs
        new_veg = get_veglag_data(tperc,tveg,lag[lagi])$veg
        veg_perc_cor[i,j] = cor(new_perc[,flname],new_veg[,flname], use = "pairwise")
        veg_obs_cor[i] = cor(new_obs[,flname],new_veg[,flname], use = "pairwise")
      }
    }
    colnames(veg_perc_cor) = percname
    rownames(veg_perc_cor) = vegname
    lag_cor[[lagi]] = rbind(veg_obs_cor,veg_perc_cor)
  }
  return(lag_cor)
}

plt_cor_lag <- function(lag_cor,lag = NULL){
  if( is.null(lag) ){
    lag = 1:length(lag_cor)
  }
  par(mfrow = c(3,3))
  par(oma = c(2,2.5,1,1), mar = c(2,2,1,1))
  vegname = c("ndvi","evi","lai")
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  
  for( vegi in 1:length(vegname)){
    for( percj in 1:length(percname)){
      te = NULL
      for( lagi in 1:length(lag) ){
        te[lagi] = lag_cor[[lagi]][vegname[vegi],percname[percj]]
      }
      plot(te, type = "o", axes = F, pch = 19, ylim = c(0,1),
           xlab = "", ylab = "", main = "", col = colorname[percj])
      axis(side = 1, at = 1:length(lag), labels = as.character(lag))
      axis(side = 2, las = 1)
      box()
      if( vegi == 1 ){
        mtext(side = 3, line = 0.5, text = percname[percj])
      }
      if( percj == 1 ){
        mtext(side = 2, line = 3, text = toupper(vegname[vegi]))
      }
      if( vegi == 3 & percj == 2 ){
        mtext(side = 1, line = 2.5, text = "Time lag")
      }
    }
  }
}


plt_cor_lag_siyu <- function(recor,lag){
  re_cor = re_p = recor$cor
  for( i in 1:length(recor$cor) ){
    re_cor[[i]] = t(recor$cor[[i]])
    re_p[[i]] = t(recor$p[[i]]<0.01)
  }
  
  
  par(mfrow = c(1,3))
  par(oma = c(2,2.5,1,1), mar = c(2,2,1,1))
  
  color = c("black",colorname)
  for( vegi in 1:length(vegname)){
    re = repp = NULL
    for( lagi in 1:length(lag) ){
      te = re_cor[[lagi]][vegname[vegi],c("obs",percname)]
      tep = re_p[[lagi]][vegname[vegi],c("obs",percname)]
      tep[which(tep == F)]=NA
      re = rbind(re,te)
      repp = rbind(repp,tep)
    }
    pp = rep(1,40)
    locidx = seq(0,-ncol(re)*0.1,-0.02)
    for( i in 1:ncol(re) ){
      plot(re[,i], type = "o", pch = 19, col = color[i], ylim = c(0,1), axes = F,
           xlab = "", ylab = "", main = "")
      lines(x = 1:41,y = pp[repp[,i]]+locidx[i], col = color[i])
      par(new = T)
    }
    axis(side = 1, at = seq(1,length(lag),10), labels = (lag-21)[seq(1,length(lag),10)] )
    axis(side = 2, las = 1)
    mtext(side = 3, line = 0.5, text = toupper(vegname[vegi]))
    box()
    par(new = F)
    if(vegi == 1){
      legend(0,0.9, legend = c("Runoff",percname), col = color, lty = 1, pch = 19, bty = "n", y.intersp = 0.5, x.intersp = 0.3)
    }
    if(vegi == 2){
      mtext(side = 1, line = 2, text = "Time lag")
    }
  }
  

}

