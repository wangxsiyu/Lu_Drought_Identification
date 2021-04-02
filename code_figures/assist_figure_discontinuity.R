plt_drynorwet_3year <- function(data_year, flname){
  # par(mfrow = c(1,4))
  # par(oma = c(2,2,1,1), mar = c(1,2,1,1))
  par(mfrow = c(4,1))
  par(oma = c(2,2,1,2), mar = c(2,2,2,2))
  
  col_t1c0f1 = rgb(0,0,1,0.7)    
  col_t0c0f0 = rgb(1,0.1,0,0.7) 
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  yr = data_year$annual$obs[,"year"]
  
###### 3 specific years
  an1 = data_year$annual$obs[,flname]
  Dryyr = which(an1 <= quantile(an1,0.2, na.rm = T))
  Normalyr = which(an1 <= quantile(an1,0.66, na.rm = T) & an1 > quantile(an1,0.2, na.rm = T))
  Wetyr = which(an1 <= quantile(an1,1, na.rm = T) & an1 > quantile(an1,0.66, na.rm = T))
  for( i in 1:3 ){
    year_h = paste(yearhydro,"yr",sep = "")
    idx = data$obs$year %in% yr[get(year_h[i])]
    plt_idx_years(data,flname,idx)
    mtext(side = 3, line = 0.5, text = yearhydro[i])
    if( i == 1 ){
      legend(-15,0.13,legend = toupper(substr(percname,6,11)), col = colorname,
             lty = 1, bty = "n", y.intersp = 0.3, x.intersp = 0.5)
    }
    if( i == 2 ){
      mtext(at = 0, side = 2, line = 2.5, text = "Mean percentile")
      mtext(at = 0, side = 4, line = 2.5, text = "Runoff", col = "grey")
    }
  }

######  average of all the data
  idx = data$obs$year>=1956 & data$obs$year<=2020
  plt_idx_years(data,flname,idx)
  mtext(side = 1, line = 2.5, text = "DOY")
  mtext(side = 3, line = 0.5, text = "1956 ~ 2020")
}


plt_3year <- function(years, data, annual, flname, starty){
  par(mfrow = c(1,4))
  par(oma = c(2,2,1,1), mar = c(2,2,1,1))
  
  yearhydro = c("Normal","Wet","Dry")
  col_t1c0f1 = rgb(0,0,1,0.7)    
  col_t0c0f0 = rgb(1,0.1,0,0.7) 
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")

###### 3 specific years
  for ( i in 1: length(years) ){
    idx = data$obs$year == years[i]
    par(plt = c(0.1,0.95,0.05,0.95))
    plt_discontinuty(idx, data, flname)
    if( i == 1 ){
      legend(-15,0.13,legend = toupper(substr(percname,6,11)), col = colorname,
             lty = 1, bty = "n", y.intersp = 0.8, x.intersp = 0.5)
      mtext(side = 2, line = 2.5, text = "Mean percentile")
    }
    if( i == 3 ){
      mtext(at = -20, side = 1, line = 2.5, text = "Consecutive dry days")
    }
    mtext(side = 3, line = 0.5, text = years[i])
    idx_y = which(annual$obs[,"year"] == years[i])
    plt_subpanel(annual, flname, idx_y, vegname, isbar = 1)
    par(plt = c(0.1,0.95,0.05,0.95))
    # plt_idx_years(data,flname,idx)

  }
######  average of all the data
  idx = data$obs$year>=starty & data$obs$year<=2020
  plt_discontinuty(idx, data, flname)
  mtext(side = 3, line = 0.5, text = paste(starty," ~ 2020",sep = ""))
  idx_ave = annual$obs[,"year"]>=starty & annual$obs[,"year"]<=2020
  plt_subpanel(annual, flname, idx_ave, vegname, isbar = 0)
}


plt_idx_years <- function(data,flname,idx){
  yearhydro = c("Normal","Wet","Dry")
  
  data_select = data$obs[idx,]
  data_doy = get_doymean(data_select)
  
  plt_discontinuty(idx, data, flname)
  par(new = T)
  plot(data_doy[,flname], type = "l", ylim = c(0,0.25),
       axes = F, xlab = "", ylab = "", main = "", col = "grey")
  axis(side = 4, las = 1, col.ticks = "grey", col.axis = "grey")
}


plt_discontinuty <- function(idx,data,flname){
  obs = data$obs[,flname]
  col_t1c0f1 = rgb(0,0,1,0.7)    
  col_t0c0f0 = rgb(1,0.1,0,0.7) 
  col_t1c1f1 = rgb(0,1,0,0.7)
  
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  for (i in 1:length(percname)){
    perc = data[[percname[i]]][,flname]
    plt_tpp(idx,obs,perc,list(max_du = 200, af_du = 200, ylime = c(0,1), color = colorname[i]))
    par(new = T)
  }
  par(new = F)
}

plt_subpanel <- function(annual, flname, idx, vegname, isbar){
  par(new = T)
  par(plt = c(0.45,0.92,0.7,0.92))
  veg = NULL
  for( i in 1:length(vegname) ){
    veg = cbind(veg,annual[[vegname[i]]][idx,flname])
  }
  #veg = cbind(veg,annual$obs[idx,flname])
  if( isbar == 1 ){
    veg = t(veg)
    xloc = seq(1.5,4,1)
    func = barplot
  }else{
    veg = veg
    xloc = seq(1,3,1)
    func = boxplot
  }
  func(veg, beside = T, axes = F, xlab = "", ylab = "", main = "",axisnames = F, ylim = c(0,0.6))
  axis(side = 1, at = xloc, labels = F)
  text(x = xloc,
       y = par("usr")[3]-0.06,
       labels = toupper(vegname),
       xpd = NA,
       ## Rotate the labels by 35 degrees.
       srt = 0,
       adj = 0.5)  
  axis(side = 2, las = 1)
  box()
  par(new = F)
}


plt_tpp <- function(idxx,obs,perccc,param = list()){
  param00 = list(max_du = 100, af_du = 100, ylime = c(0,1), color = "black")
  param = modifyList(param00,param)
  cdpm_siyu = get_cumulativedays(obs==0)
  cdpm_siyu_re = calc_0value(obs==0,"reverse")
  cdpm_tot = cdpm_siyu + cdpm_siyu_re
  idx_siyu = cdpm_tot > param$max_du
  idx_l = which(idx_siyu & cdpm_siyu == 1 & idxx)
  idx_bf = idx_l# - 10
  idx_af = idx_l + param$af_du
  tpp = matrix(NA,length(idx_l),param$af_du+1) #+11)
  for( i in 1:length(idx_l)){
    tpp[i,] = perccc[idx_bf[i]:idx_af[i]]
    #plot(x = -10:param$af_du, y = tpp[i,],type = "l", ylim = param$ylime, col = "grey")
    #par(new = T)
  }
  tpp_cm = colMeans(tpp,na.rm = T)
  plot(x = 0:param$af_du, y = tpp_cm,type = "l", xlim = c(0,param$af_du), ylim = param$ylime, col = param$color,las = 1,
       xlab = "", ylab = "", main = "")
}

get_cumulativedays<- function(x){
  y = matrix(NaN, length(x),1)
  idnan = which(!is.na(x))
  y[idnan] = 0
  y[idnan[x[idnan]>0]] = 1
  count = 0
  for (i in 1:length(y)){
    if (is.na(y[i]) || y[i] == 0){
      count = 0
    } else {
      count = count + 1
      y[i] = count
    }
  }
  y = y[,1]
  return(y)
}

calc_0value <- function(Q0, param_cdpm = 'linear'){
  library(pracma)
  cd = get_cumulativedays(Q0)
  if (param_cdpm == 'triangular'){
    cd_rev = get_cumulativedays(Q0[length(Q0):1])
    cd_rev = cd_rev[length(cd_rev):1]
    # cd = abs((cd - cd_rev)/2) 
    te = cbind(cd, cd_rev)
    id = max.col(-te, ties.method ="first")
    cd = arrayfun(function(x){te[x, id[x]]}, 1:length(id))
  }else if(param_cdpm == 'reverse'){
    cd_rev = get_cumulativedays(Q0[length(Q0):1])
    cd_rev = cd_rev[length(cd_rev):1]
    cd = cd_rev
  }
  cd[cd == 0] = NaN
  return(cd)
}

get_doymean <- function(data_select){
  doy = get_dayid(data_select$month,data_select$day)
  year = unique(data_select$year)
  nyear = max(365, max(doy,na.rm = T))
  doy_mean = matrix(NA,nyear,ncol(data_select))
  for ( dyi in 1:nyear ){
    idx = which(doy == dyi)
    doy_mean[dyi,] = colMeans(data_select[idx,], na.rm = T)
  }
  colnames(doy_mean) = colnames(data_select)
  datacol = setdiff(colnames(doy_mean),c("year","month","day","X"))
  data_doy = cbind(doy = 1:nyear, doy_mean[,datacol])
  return(data_doy)
}


