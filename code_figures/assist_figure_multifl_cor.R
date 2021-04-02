get_obs_veg_cor <- function(data, flname, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  vegname = c("ndvi","evi","lai")
  daily_cor = matrix(NA,length(vegname),1)
  sum_nontobs = NULL
  for (i in 1:length(vegname)){
    tobs = data[["obs"]][,flname]
    tveg = data[[vegname[i]]][, flname_veg]
    daily_cor[i] = cor(tobs, tveg, use = "pairwise")
    sum_nontobs[i] = length(which(!is.na(tobs)))
  }
  rownames(daily_cor) = vegname
  return(daily_cor)
}

get_perc_veg_cor <- function(data, flname, percname, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  vegname = c("ndvi","evi","lai")
  #percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  daily_cor = matrix(NA,length(vegname),length(percname))
  for (i in 1:length(vegname)){
    for (j in 1:length(percname)){
      tperc = data[[percname[j]]][,flname]
      tveg = data[[vegname[i]]][, flname_veg]
      daily_cor[i,j] = cor(tperc, tveg, use = "pairwise")
    }
  }
  colnames(daily_cor) = percname
  rownames(daily_cor) = vegname
  return(daily_cor)
}

plt_multi_fl_fac <- function(factor,flname, color = "black"){
  ymin = min(factor,na.rm = T)
  ymax = max(factor,na.rm = T)
  plot(factor, type = "o", col = color, ylim = c(ymin,ymax), xlim = c(0.5,length(flname)+0.5), pch = 19,
       xlab = "", ylab = "", main = "", axes = F)
  axis(side = 1, at = 1:length(flname), labels = F)
  text(x = 1:length(flname),
       y = par("usr")[3]-0.03,
       labels = flname,
       xpd = NA,
       ## Rotate the labels by 35 degrees.
       srt = 30,
       adj = 1,
       cex = 1)
  axis(side = 2, las = 1)
  box()
}

plt_multi_fl <- function(func, data, flname, isres=0, isperc=1){
  vegname = c("ndvi","evi","lai")
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  drydayname = c("dryday_t1c0f1","dryday_t0c0f0","dryday_t1c1f1")
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  
  if(isperc == 1){
    varname = percname
  }else{
    varname = drydayname
  }
  veg_fl = NULL
  obs = matrix(NA,length(flname),length(vegname))
  fl = matrix(list(),1,length(flname))
  for( i in 1:length(vegname) ){
    for( fli in 1:length(flname) ){
      if(isres == 0){
        fl[[fli]] = func(data, flname[fli], varname)
      }else if(isres == 1){
        fl[[fli]] = t(func(data, flname[fli])$cohen_d)
      }
      veg_fl = rbind(veg_fl,fl[[fli]][i,])
      obs[fli,] = get_obs_veg_cor(data, flname[fli])
    }
  }
  rownames(veg_fl) = rep(vegname,each=length(flname))
  colnames(obs) = vegname
  veg_obs = list()
  veg_obs$veg = veg_fl
  veg_obs$obs = obs
  
#### line plot
  par(mfrow = c(3,1))
  par(oma = c(2,2,1,1), mar = c(2,2,1,1))
  plt_lineplt_compare(vegname, percname, flname, veg_obs)
  
#### pie plot
  perc_percent = plt_pieplt_compare(veg_obs,vegname)
  
  return(perc_percent)
}

get_pieplt_compare <- function(veg_obs,vegname){
  veg_fl = veg_obs$veg
  obs = veg_obs$obs
  veg_mx = perc_percent = NULL
  for( i in 1:length(vegname) ){
    idx = which(rownames(veg_fl) == vegname[i])
    re = veg_fl[idx,]
    mx = percent = NULL
    for( j in 1:nrow(re)){
      mx[j] = which.max(re[j,])
    }
    for( jj in 1:ncol(re) ){
      percent[jj] = length(which(mx == jj))
    }
    veg_mx = cbind(veg_mx,mx)
    perc_percent = rbind(perc_percent,percent)
  }
  colnames(veg_mx) = vegname
  colnames(perc_percent) = colnames(veg_fl)
  rownames(perc_percent) = vegname
  return(perc_percent)
### plot
}

plt_lineplt_compare <- function(vegname, percname, flname, veg_obs){
  veg_fl = veg_obs$veg
  obs = veg_obs$obs
  for( i in 1:length(vegname) ){
    idx = which(rownames(veg_fl) == vegname[i])
    ymin = min(veg_fl[idx,],na.rm = T)
    ymax = max(veg_fl[idx,],na.rm = T)
    for( j in 1:length(percname) ){
      plot(veg_fl[idx,j], type = "o", col = colorname[j], ylim = c(ymin,ymax), xlim = c(0.5,length(flname)+0.5), pch = 19,
           xlab = "", ylab = "", main = "", axes = F)
      lines(obs[,i],type = "o", lty = 2, col = "grey",ylim = c(ymin,ymax), xlim = c(0.5,length(flname)+0.5), pch = 19,
           xlab = "", ylab = "", main = "", axes = F)
      par(new = T)
    }
    if( i == 3 ){
      axis(side = 1, at = 1:length(idx), labels = F)
      text(x = 1:length(idx),
           y = par("usr")[3]-0.03,
           labels = flname,
           xpd = NA,
           ## Rotate the labels by 35 degrees.
           srt = 30,
           adj = 1,
           cex = 1.2) 
      mtext(side = 1, line = 2.5, text = "Flume")
    }
    axis(side = 2, las = 1)
    box()
    legend(0,0.6, legend = toupper(vegname[i]), bty = "n")
    if( i == 1 ){
      legend(0.5,0.2,legend = percname, col = colorname, lty = 1, pch = 19, bty = "n", x.intersp = 0.3)
    }
    if( i == 2 ){
      mtext(side = 2, line = 2.5, text = "Correlation")
    }
  }
}

plt_barplot_compare <- function(vegname, percname, flname, veg_fl){
  color = rep(colorname,each=3) #colorname 
  for( i in 1:length(vegname) ){
    idx = which(rownames(veg_fl) == vegname[i])
    re = veg_fl[idx,] #fl[[i]]
    barplot(re, beside = T,ylim = c(-0.01,0.65), 
            cex.names = 2,axes = F,axisnames = F,xlab = NULL,ylab = NULL,col = color)
    axis(side = 1, at = seq(1.5,11.5,1), labels = F)
    text(x = seq(1.5,11.5,1)[-c(4,8)],
         y = par("usr")[3]-0.03,
         labels = rep(flname,3),
         xpd = NA,
         ## Rotate the labels by 35 degrees.
         srt = 35,
         adj = 0.6,
         cex = 1.2)  
    axis(side = 2, las = 1)
    box()
    legend(0,0.6, legend = toupper(vegname[i]), bty = "n")
    if(i == 1){
      legend(0,0.55,legend = percname, col = colorname, pch = 15, bty = "n", x.intersp = 0.3)
      mtext(side = 2, line = 2.5, text = "Correlation")
    }
    if( i == 2 ){
      mtext(side = 1, line = 2.5, text = "Flume")
    }
  }
}