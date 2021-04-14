source("assist_figure_daily.R")
plt_dailycor <- function(data,flname){
  dailycor = calc_dailycor(data,flname)
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  par(mfrow = c(3,1))
  par(oma = c(1,1,1,1), mar = c(1,1,1,1))
  for( i in 1:nrow(dailycor) ){
    for( j in 1:ncol(dailycor) ){
      plot(dailycor[[i,j]], col = colorname[j], ylab = "",xlab = "", type = "l", main = toupper(vegname[i]), ylim = c(0,1), las = 1)
      par(new = T)
      plt_doy_fr_0(data,flname)
      par(new = T)
    }
    par(new = F)
  }
  legend("topleft", legend = percname, col = colorname, lty = 1,  bty = "n", x.intersp = 0.3)
}

calc_dailycor <- function(data,flname,flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  daily_cor = matrix(list(),length(vegname),length(percname))
  doy = get_dayid(data$obs$month,data$obs$day)
  day365 = max(365, max(doy))
  for (i in 1:length(vegname)){
    for (j in 1:length(percname)){
      tperc = data[[percname[j]]][,flname]
      tveg = data[[vegname[i]]][,flname_veg]
      #calculate daily veg
      idx = which(!is.na(tveg))
      daily_veg = approx(x = idx, y = tveg[idx], xout = 1:length(tveg))$y
      #calculate daily cor
      daycor = NULL
      for( dayi in 1: day365){
        idx_d = which(doy == dayi)
        daycor[dayi] = cor(tperc[idx_d], daily_veg[idx_d], use = "pairwise")
      }
      daily_cor[[i,j]] = daycor
    }
  }
  colnames(daily_cor) = percname
  rownames(daily_cor) = vegname
  return(daily_cor)
}

plt_selectday <- function(data,flname,dayi,flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  daily_cor = matrix(list(),length(vegname),length(percname))
  doy = get_dayid(data$obs$month,data$obs$day)
  day365 = max(365, max(doy))
  par(mfrow = c(3,1))
  par(oma = c(1,1,1,1), mar = c(1,1,1,1))
  for (i in 1:length(vegname)){
    tveg = data[[vegname[i]]][,flname_veg]
    #calculate daily veg
    idx = which(!is.na(tveg))
    daily_veg = approx(x = idx, y = tveg[idx], xout = 1:length(tveg))$y
    idx_d = which(doy == dayi & !is.na(daily_veg))
    plot(x = 1:length(idx_d), y = daily_veg[idx_d]/max(daily_veg[idx_d],na.rm = T), col = "black", type = "o", ylim = c(0,1), las = 1)
    for (j in 1:length(percname)){
      tperc = data[[percname[j]]][,flname]
      par(new = T)
      plot(x = 1:length(idx_d), y = tperc[idx_d], col = colorname[j], type = "o", ylim = c(0,1),las = 1)
      #which(tperc[idx_d] > 0.5)
    }
    par(new = F)
  }
}

# plt_selectday(data,flname,75)
# plt_selectday(data,flname,175)
# plt_selectday(data,flname,220)




plt_selectday_scatter <- function(data,flname,dayi,flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  id0 = data$obs[,flname] >= 0
  percname = c("perc_t1c0f1","perc_t0c0f1","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  daily_cor = matrix(list(),length(vegname),length(percname))
  doy = get_dayid(data$obs$month,data$obs$day)
  day365 = max(365, max(doy))
  par(mfrow = c(3,3))
  par(oma = c(1,1,1,1), mar = c(1,1,1,1))
  for (i in 1:length(vegname)){
    tveg = data[[vegname[i]]][,flname_veg]
    #calculate daily veg
    idx = which(!is.na(tveg))
    daily_veg = approx(x = idx, y = tveg[idx], xout = 1:length(tveg))$y
    idx_d = which(doy == dayi & !is.na(daily_veg) & id0)
    #plot(x = 1:length(idx_d), y = daily_veg[idx_d]/max(daily_veg[idx_d],na.rm = T), col = "black", type = "l", ylim = c(0,1), las = 1)
    for (j in 1:length(percname)){
      tperc = data[[percname[j]]][,flname]
      #plot(y = daily_veg[idx_d], x = tperc[idx_d], pch = 19, col = colorname[j], xlim = c(0,1),las = 1)
      plt_trendline(tperc[idx_d], daily_veg[idx_d], colorname[j])
    }
    par(new = F)
  }
}

# plt_selectday_scatter(data,flname,75)
# plt_selectday_scatter(data,flname,175)
# plt_selectday_scatter(data,flname,220)

plt_nacor <- function(data,flname){
  t0 = calc_nacor(data,flname,option = 0)
  dailycor = t0$daily_cor;
  t1 = calc_nacor(data,flname,option = 1)
  dailycor1 = t1$daily_cor;
  d0 = calc_dailycor(data,flname)
  
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  par(mfrow = c(3,3))
  par(oma = c(1,1,1,1), mar = c(1,1,1,1))
  for( i in 1:nrow(dailycor) ){
    for( j in 1:ncol(dailycor) ){
      plot(dailycor[[i,j]], col = colorname[j], ylab = "",xlab = "", type = "l", main = toupper(vegname[i]), ylim = c(0,1), las = 1)
      par(new = T)
      #plt_doy_fr_0(data,flname)
      #par(new = T)
      
      plot(t0$n0[[i,j]]/max(t0$n0[[i,j]]), col = "gray", ylab = "",xlab = "", type = "l", ylim = c(0,1), las = 1)
      par(new = T)
      plot(t0$nlongdry[[i,j]]/max(t0$nlongdry[[i,j]]), col = "black", ylab = "",xlab = "", type = "l", ylim = c(0,1), las = 1)
      par(new = T)
    }
    par(new = F)
    
    for( j in 1:ncol(dailycor1) ){
      plot(dailycor1[[i,j]], col = colorname[j], ylab = "",xlab = "", type = "l", main = toupper(vegname[i]), ylim = c(0,1), las = 1)
      par(new = T)
    #  plt_doy_fr_0(data,flname)
    #  par(new = T)
      plot(t1$n0[[i,j]]/max(t1$n0[[i,j]]), col = "gray", ylab = "",xlab = "", type = "l", ylim = c(0,1), las = 1)
      par(new = T)
      plot(t1$nlongdry[[i,j]]/max(t1$nlongdry[[i,j]]), col = "black", ylab = "",xlab = "", type = "l", ylim = c(0,1), las = 1)
      par(new = T)
    }
    par(new = F)
    
    
    for( j in 1:ncol(d0) ){
      plot(d0[[i,j]], col = colorname[j], ylab = "",xlab = "", type = "l", main = toupper(vegname[i]), ylim = c(0,1), las = 1)
      par(new = T)
      #  plt_doy_fr_0(data,flname)
      #  par(new = T)
      plot(t0$n0[[i,j]]/max(t0$n0[[i,j]]), col = "gray", ylab = "",xlab = "", type = "l", ylim = c(0,1), las = 1)
      par(new = T)
      plot(t0$nlongdry[[i,j]]/max(t0$nlongdry[[i,j]]), col = "black", ylab = "",xlab = "", type = "l", ylim = c(0,1), las = 1)
      par(new = T)
    }
    par(new = F)
  }
  legend("topleft", legend = percname, col = colorname, lty = 1,  bty = "n", x.intersp = 0.3)
}


calc_nacor <- function(data,flname,option = 0, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  percname = c("perc_t1c0f1","perc_t0c0f1","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  daily_cor = n0 = nlongdry = matrix(list(),length(vegname),length(percname))
  doy = get_dayid(data$obs$month,data$obs$day)
  day365 = max(365, max(doy))
  if( option == 0 ){
    id0 = data$obs[,flname] == 0
  }else{
    id0 = data$obs[,flname] > 0
  }
  tobs = data$obs[, flname]
  for (i in 1:length(vegname)){
    tveg = data[[vegname[i]]][,flname_veg]
    #calculate daily veg
    idx = which(!is.na(tveg))
    daily_veg = approx(x = idx, y = tveg[idx], xout = 1:length(tveg))$y
    for (j in 1:length(percname)){
      tperc = data[[percname[j]]][,flname]
      #calculate daily cor
      daycor = t1 = t3 = NULL
      # te = zeros(,1)
      for( dayi in 1: day365){
        idx_d = which(doy == dayi & id0)
        if (length(idx_d) > 1){
          idx_d = idx_d[2:length(idx_d)] 
        }
        t1[dayi] = length(idx_d);
        t3[dayi] = 0.5;
        if( length(idx_d) == 0 ){
          daycor[dayi] = NA
        }else{
          daycor[dayi] = cor(tperc[idx_d], daily_veg[idx_d], use = "pairwise", method = "pearson")
        }
      }
      daily_cor[[i,j]] = daycor
      n0[[i,j]] = t1
      nlongdry[[i,j]] = t3
    }
  }
  colnames(daily_cor) = percname
  rownames(daily_cor) = vegname
  out = list();
  out$daily_cor = daily_cor;
  out$n0 = n0;
  out$nlongdry = nlongdry;
  return(out)
}


