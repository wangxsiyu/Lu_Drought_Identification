plt_motiv <- function(data,annual,flname){
  par(mfrow = c(2,4))
  par(oma = c(1.5,2.5,1,1), mar = c(2,2.5,1,2))

  an1 = get_table(annual,flname)
  idx_p = seq(which(!is.na(an1$obs))[1],nrow(an1),1)
  idx_label = c(idx_p[seq(1,length(idx_p),10)],nrow(an1))
  
  ############ Panel Runoff yearly mean ---- barplot
  plt_obs_ts(an1,idx_p,idx_label)
  #legend(-3,ymax*1.2, legend = "a)", bty = "n")
  
  ############ Panel Fraction of zero VS DOY
  td = data$obs
  obs_doy_mean = get_var_doy(td)
  plot(obs_doy_mean[,flname], type = "l", xlab ="", ylab = "", axes = F, col = "black")
  axis(side = 2, las = 1, col.ticks = "black", col.axis = "black")
  mtext(side = 2, line = 3.2, text = "Daily mean runoff")
  legend(-10,0.05, y.intersp = 0.8, x.intersp = 0.2, legend = c("Zero fraction"), cex = 1.2,
         col = c("grey"), lty = 1, bty = "n")
  par(new = T)
  plt_doy_fr_0(data,flname)
  axis(side = 4, las = 1, col.ticks = "grey", col.axis = "grey")
  

  ############ Panel correlation of daily runoff and NDVI
  plt_runoff_veg(data, flname, "Non-zero daily runoff")#, rxloc = 0.18,ryloc = 0.2)
  mtext(side = 3, line = 0.2, text = "NDVI")
  
  ############ Panel non-zero / zero flow veg distribution ---- density
  plt_veg_density(data, flname)
  
  ############ Panel Veg time series ---- lineplot
  plt_veg_ts(an1,idx_p,idx_label)
  #legend(-3,ymax2*1.2, legend = "b)", bty = "n")
  legend("bottomleft", legend = c("NDVI","EVI","LAI"), col = c("green","red","blue"), cex = 1.2, lty = 1, bty = "n")
  
  ############ Panel Vegetation VS DOY
  plt_veg_doy(data, flname)
  axis(side = 1)
  axis(side = 2, las = 1)
  box()
  mtext(side = 1, line = 2.5, text = "DOY")
  mtext(side = 2, line = 2.5, text = "Variable")
  
    
  ############ Panel correlation of daily runoff and NDVI
  plt_runoff_veg(annual, flname, "Yearly runoff")#, rxloc = 0.18,ryloc = 0.02)
  

}

get_table <- function(x,name){
  tn = names(x)
  out = matrix(NA,nrow(x[[tn[2]]]),length(tn))
  for( i in 1:length(tn) ){
    ttn = tn[i]
    out[,i] = x[[ttn]][,name]
  }
  colnames(out) = tn
  year = x[[tn[1]]][,"year"]
  out = cbind(year,out)
  return(as.data.frame(out))
}

get_year_fr_0 <- function(data,flname){
  idx_start = which(!is.na(data$obs[,flname]))[1]
  year_start = data$obs$year[idx_start]
  year = unique(data$obs$year)
  year_new = year_start : year[length(year)]
  fr_0 = NULL
  for( yeari in 1:length(year_new)){
    idx = which(data$obs$year == year_new[yeari])
    fr_0[yeari] = length(which(data$obs[idx,flname] == 0))/length(idx)
  }
  names(fr_0) = year_new
  return(fr_0)
}

get_var_doy <- function(td){
  ndata = setdiff(colnames(td),c("year","month","day"))
  doy = get_dayid(td$month,td$day)
  day365 = max(365, max(doy))
  doy_mean = matrix(NA,day365,length(ndata))
  for( fli in 1:length(ndata) ){
    for( dayi in 1:day365){
      id = which(doy == dayi)
      doy_mean[dayi,fli] = mean(td[id,ndata[fli]],na.rm = T)
    }
  }
  colnames(doy_mean) = ndata
  return(doy_mean)
}


plt_trendline <- function(a,b,rxloc,ryloc){
  require(basicTrendline)
  idx = !is.na(a) & !is.na(b)
  a = a[idx]
  b = b[idx]
  trendline(x = a,y = b, model = "line2P",summary = F,show.Rsquare = T, show.pvalue = T, show.equation = F, 
            pch = 16, xlab = "",ylab = "",eDigit = 2, eSize = 1.8)#, ePos.x = rxloc, ePos.y = ryloc)
}

plt_obs_ts <- function(an1,idx_p,idx_label){
  ymax = max(an1[idx_p,"obs"],na.rm = T)*1.05
  barplot(as.matrix(an1[idx_p,"obs"]),ylim = c(0,ymax), axes = F,beside = T, col = "dark grey")
  axis(side = 1, at = c(seq(1.5,length(idx_p)+0.5,10),length(idx_p)+0.5), labels = an1[idx_label,"year"]) #length(idx_p)%/%10
  axis(side = 1, at = seq(1.5,length(idx_p)+0.5,1), labels = F, tck = -0.02)
  axis(side = 2, las = 1)
  mtext(side = 2, line = 3.2, text = "Yearly mean runoff")
  box()
}

plt_veg_ts <- function(an1,idx_p,idx_label){
  ymin = min(an1[idx_p,c("ndvi","evi","lai")],na.rm = T)
  ymax = max(an1[idx_p,c("ndvi","evi","lai")],na.rm = T)
  plot(an1[idx_p,"ndvi"], col = "green", type = "l", ylim = c(ymin,ymax*1.05), axes = F, xlab = "", ylab = "", main = "")
  lines(an1[idx_p,"evi"], col = "red")
  lines(an1[idx_p,"lai"], col = "blue")
  axis(side = 1, at = c(seq(1,length(idx_p),10),length(idx_p)), labels = an1[idx_label,"year"]) #length(idx_p)%/%10
  axis(side = 1, at = seq(1,length(idx_p),1), labels = F, tck = -0.02)
  axis(side = 2, las = 1)
  mtext(side = 2, line = 3.2, text = "Vegetation")
  box()
}

plt_veg_doy <- function(data, flname){
  vegname = c("ndvi","evi","lai")
  veg_doy_mean = NULL
  for (i in 1:length(vegname)){
    tveg = data[[vegname[i]]]
    veg_mean = get_var_doy(tveg)[, flname]
    veg_doy_mean = cbind(veg_doy_mean,veg_mean)
  }
  colnames(veg_doy_mean) = vegname
  
  # plot
  color = c("green","red","blue")
  ymax = max(veg_doy_mean,na.rm = T)
  ymin = min(veg_doy_mean,na.rm = T)
  for (i in 1:length(vegname)){
    td = veg_doy_mean[,vegname[i]]
    id = which(!is.na(td))
    #plot(td,col = color[i], type = "l", xlab ="", ylab = "", axes = F, ylim = c(ymin,ymax*1.05) )
    func  = smooth.spline(x = id, y = td[id])
    plot(func, col = color[i], type = "l", xlab ="", ylab = "", axes = F, ylim = c(ymin,ymax*1.05))
    par(new = T)
  }
  par(new = F)
  return(ymax)
}

plt_runoff_veg <- function(data, flname, mtext, rxloc, ryloc, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  #par(mfrow = c(1,3))
  #par(oma = c(1,2,1,1), mar = c(2,2,2,1))
  
  idx_no0 = which(data$obs[,flname] != 0)
  tobs = data$obs[idx_no0,flname]
  vegname = c("ndvi") #c("ndvi","evi","lai")
  for (i in 1:length(vegname)){
    tveg = data[[vegname[i]]][idx_no0, flname_veg]
    plt_trendline(tveg,tobs)#,rxloc,ryloc)
    #mtext(side = 3, line = 0.2, text = toupper(vegname[i]))
    if( i == 1 ){
      mtext(side = 2, line = 3.2, text = mtext)
    }
  }
}

plt_veg_density <- function(data, flname, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  # par(mfrow = c(1,3))
  # par(oma = c(1,2,1,1), mar = c(2,2,2,1))
  
  idx_no0 = which(data$obs[,flname] != 0)
  idx_00 = which(data$obs[,flname] == 0)
  
  vegname = c("ndvi") #c("ndvi","evi","lai")
  for (i in 1:length(vegname)){
    tveg = data[[vegname[i]]][, flname_veg]
    d_no0 = density(tveg[idx_no0],na.rm = T)
    d_00 = density(tveg[idx_00],na.rm = T)
    plot(d_no0,ylim = c(min(d_no0$y,d_00$y),max(d_no0$y,d_00$y)*1.05), type = "l", axes = F, main = "", ylab = "", xlab = "")
    lines(d_00, col = "red")
    axis(side = 1)
    axis(side = 2,las = 1)
    box()
    if( i == 1 ){
      legend(0.2,12, legend = c("Non-zero flow days","Zero flow days"), col = c("black","red"),
             lty = 1, bty = "n", cex = 1.2)
      mtext(side = 2, line = 2.2, text = "Vegetation density")
    }
    #mtext(side = 3, line = 0.8, text = toupper(vegname[i]), cex = 0.8)
  }
  
}



