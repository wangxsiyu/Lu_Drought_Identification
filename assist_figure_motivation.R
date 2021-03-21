


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

plt_year_ts <-function(annual,flname, year_fr_0){
  par(mfrow = c(3,1))
  par(oma = c(1,2,1,1), mar = c(2,2.5,1,1))
  
  an1 = get_table(annual,flname)
  idx_p = seq(which(!is.na(an1$obs))[1],nrow(an1),1)
  idx_label = c(idx_p[seq(1,length(idx_p),10)],nrow(an1))
  
  ############ Panel a) Runoff yearly mean ---- barplot
  ymax = max(an1[idx_p,"obs"],na.rm = T)*1.1
  barplot(as.matrix(an1[idx_p,"obs"]),ylim = c(0,ymax), axes = F,beside = T, col = "dark grey")
  axis(side = 1, at = c(seq(1.5,length(idx_p)+0.5,10),length(idx_p)+0.5), labels = an1[idx_label,"year"]) #length(idx_p)%/%10
  axis(side = 1, at = seq(1.5,length(idx_p)+0.5,1), labels = F, tck = -0.02)
  axis(side = 2, las = 1)
  mtext(side = 2, line = 3.1, text = "Runoff", cex = 0.8)
  box()
  legend(-3,ymax*1.2, legend = "a)", bty = "n")
  
  ############ Panel b) Veg time series ---- lineplot
  ymin = min(an1[idx_p,c("ndvi","evi","lai")],na.rm = T)
  ymax = max(an1[idx_p,c("ndvi","evi","lai")],na.rm = T)
  plot(an1[idx_p,"ndvi"], type = "l", ylim = c(ymin,ymax), axes = F, xlab = "", ylab = "", main = "")
  lines(an1[idx_p,"evi"], col = "red")
  lines(an1[idx_p,"lai"], col = "blue")
  axis(side = 1, at = c(seq(1,length(idx_p),10),length(idx_p)), labels = an1[idx_label,"year"]) #length(idx_p)%/%10
  axis(side = 1, at = seq(1,length(idx_p),1), labels = F, tck = -0.02)
  axis(side = 2, las = 1)
  mtext(side = 2, line = 3, text = "Vegetation", cex = 0.8)
  box()
  legend(-3,ymax2*1.2, legend = "b)", bty = "n")
  legend("bottomleft", legend = c("NDVI","EVI","LAI"), col = c("black","red","blue"), lty = 1)
  
  ############ Panel e) Zero fraction as a function of year ---- barplot
  ymax3 = max(year_fr_0)*1.1
  plot(density(year_fr_0), axes = F, main = "", xlab ="", ylab = "")
  axis(side = 1)
  axis(side = 2, las = 1)
  mtext(side = 2, line = 2.4, text = "Fraction of zero runoff", cex = 0.8)
  box()
  legend(-3,ymax3*1.2, legend = "c)", bty = "n")
}

plt_trendline <- function(a,b,mainname = NULL){
  require(basicTrendline)
  idx = !is.na(a) & !is.na(b)
  a = a[idx]
  b = b[idx]
  trendline(x = a,y = b, model = "line2P",summary = F,show.Rsquare = T, show.pvalue = T, show.equation = F, 
            pch = 16, xlab = "",ylab = "")
  mtext(side = 3, line = 0.1, text = mainname, cex = 0.8)
}

plt_runoff_veg <- function(data, flname, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  par(mfrow = c(1,3))
  par(oma = c(1,2,1,1), mar = c(2,2,2,1))
  
  idx_no0 = which(data$obs[,flname] != 0)
  tobs = data$obs[,flname]
  vegname = c("ndvi","evi","lai")
  for (i in 1:length(vegname)){
    tveg = data[[vegname[i]]][, flname_veg]
    plt_trendline(tveg,tobs)
    mtext(side = 3, line = 0.8, text = toupper(vegname[i]), cex = 0.8)
    if( i == 1 ){
      mtext(side = 2, line = 2.5, text = "Runoff", cex = 0.8)
    }
  }
}

plt_veg_density <- function(data, flname){
  par(mfrow = c(1,3))
  par(oma = c(1,2,1,1), mar = c(2,2,2,1))
  
  idx_no0 = which(data$obs[,flname] != 0)
  idx_00 = which(data$obs[,flname] == 0)
  
  vegname = c("ndvi","evi","lai")
  for (i in 1:length(vegname)){
    tveg = data[[vegname[i]]][, flname_veg]
    d_no0 = density(tveg[idx_no0],na.rm = T)
    d_00 = density(tveg[idx_00],na.rm = T)
    plot(d_no0,ylim = c(min(d_no0$y,d_00$y),max(d_no0$y,d_00$y)), type = "l", axes = F, main = "", ylab = "", xlab = "")
    lines(d_00, col = "red")
    axis(side = 1)
    axis(side = 2,las = 1)
    box()
    if( i == 1 ){
      legend(0.2,12, legend = c("Non-zero flow days","Zero flow days"), col = c("black","red"),
             lty = 1, bty = "n", cex = 0.8)
      mtext(side = 2, line = 2, text = "Vegetation density")
    }
    mtext(side = 3, line = 0.8, text = toupper(vegname[i]), cex = 0.8)
  }
  
}



