
calc_lag_annual <- function(data){
  result = list()
  # compute annual (perc/neg)
  data_name = names(data)
  annual = list()
  annual_lag = matrix(list(),1,365)
  for( lagi in 1:365 ){
    for( di in 1:length(data_name) ){
      td = data[[data_name[di]]]
      annual[[data_name[di]]] = get_year_lag(td,lagi)
    }
    ## dryday x perc x 8
    for (t in 0:1){
      for (c in 0:1){
        for (f in 0:1){
          percname = sprintf("perc_t%dc%df%d", t, c, f)
          drydayname = sprintf("dryday_t%dc%df%d", t, c, f)
          annual[[drydayname]] = get_year_dry_lag(data[[percname]],lagi)
        }
      }
    }
    annual_lag[[lagi]] = annual
    print(sprintf('processing lag %d',lagi))
  }
  result$annual_lag = annual_lag
  return(result)
}

get_year_lag <- function(td,lag){
  years = unique(td$year)
  doy = get_dayid(td$month,td$day)
  yearid = which(doy == lag)
  out = matrix(NA,length(years),ncol(td))
  for( yi in 1:(length(yearid)-1)){
    id = yearid[yi]:(yearid[yi+1]-1)
    out[yi,] = colMeans(td[id,],na.rm = T)
  }
  colnames(out) = colnames(td)
  data_col = setdiff(colnames(out),c("month","day"))
  out[,1] = years
  out = out[,data_col]
  return(out)
}

get_year_dry_lag <- function(td,lag){
  years = unique(td$year)
  doy = get_dayid(td$month,td$day)
  yearid = which(doy == lag)
  ndata = setdiff(colnames(td),c("year","month","day"))
  dry_y = matrix(NA,length(years),length(ndata)+1)
  for( yi in 1:(length(yearid)-1)){
    id = yearid[yi]:(yearid[yi+1]-1)
    for( fli in 1:length(ndata)){
      if( length(which(is.na(td[id,ndata[fli]])))>360 ){
        dry_y[yi,fli+1] = NA
      }else{
        dry_y[yi,fli+1] = length(which(td[id,ndata[fli]]<0.2))
      }
    }
  }
  colnames(dry_y) = c("year",ndata)
  dry_y[,1] = years
  return(dry_y)
}

get_veg_perc_lag_cor <- function(data_lag,flname,percname){
  vegname = c("ndvi","evi","lai")
  veg_perc_cor = NULL
  veg_perc_cor_lag = matrix(list(),length(vegname),length(percname))
  for (i in 1:length(vegname)){
    for (j in 1:length(percname)){
      for( lagi in 1:365 ){
        tveg = data_lag[[lagi]][[vegname[i]]][,flname]
        tperc = data_lag[[lagi]][[percname[j]]][,flname]
        veg_perc_cor[lagi] = cor(tveg,tperc,use = "pairwise")
      }
      veg_perc_cor_lag[[i,j]] = veg_perc_cor
    }
  }
  colnames(veg_perc_cor_lag) = percname
  rownames(veg_perc_cor_lag) = vegname
  return(veg_perc_cor_lag)
}

plt_veg_lag_cor <- function(data_lag,flname,percname){
  veg_lag = get_veg_perc_lag_cor(data_lag,flname,percname)
  
  par(mfrow = c(1,3))
  par(oma = c(2,2,1,1), mar = c(1.5,2,1,1))
  
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  vegname = c("ndvi","evi","lai")
  mename = c("t1c0f1","t0c0f0","t1c1f1")
  
  ymin = min(unlist(veg_lag))
  ymax = max(unlist(veg_lag))
  for( j in 1:nrow(veg_lag) ){
    for( i in 1:ncol(veg_lag) ){
      plot(x = 1:365, y = unlist(veg_lag[j,i]), type= "o", ylim = c(ymin,ymax),
           las = 1, xlab = "", ylab = "", main = "", pch = 16, col = colorname[i])
      par(new = T)
    }
    par(new = F)
    mtext(side = 3, line = 0.5, text = toupper(vegname[j]))
    if( j == 1 ){
      mtext(side = 2, line = 2.2, text = "Correlation")
      legend(200,0.05, legend = toupper(mename), col = colorname,
             pch = 16, lty = 1, bty = "n")
    }
    if( j == 2 ){
      mtext(side = 1, line = 2.2, text = "Start day of a year")
    }
  }
}




