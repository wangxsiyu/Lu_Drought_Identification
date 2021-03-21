plt_flume_daily <- function(data, flname,percname, flname_veg = NULL){
    if (is.null(flname_veg)){
      flname_veg = flname
    }
    par(mfrow = c(3,3))
    par(oma = c(1,2,1,1), mar = c(2,2,1,1))
    #percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
    vegname = c("ndvi","evi","lai")
    for (i in 1:length(vegname)){
      for (j in 1:length(percname)){
        tperc = data[[percname[j]]][,flname]
        tveg = data[[vegname[i]]][, flname_veg]
        plt_trendline(tperc, tveg, paste(percname[j], flname, sep = ","))
        if (j == 1){
          mtext(side = 2, line = 2.5, text = toupper(vegname[i]), cex = 0.8)
        }
      }
    }
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

plt_idx_daily <- function(data,idx,flname, percname, vegname, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  tperc = data[[percname]][idx,flname]
  tveg = data[[vegname]][idx, flname_veg]
  plt_trendline(tperc, tveg)
}

plt_monsoon_zero <- function(data, flname, percname, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  idx_n = list()
  idx_n$idx = 1:length(data$obs[,flname])
  #zero flow
  idx_n$idx_00 = which(data$obs[,flname] == 0) 
  #zero flow in monsoon
  idx_n$idx_mons0 = idx_00[which(data$obs[idx_00,"month"] >=6 & data$obs[idx_00,"month"] <=9)]
  #zero flow in non-monsoon
  idx_n$idx_nomons0 = idx_00[which(data$obs[idx_00,"month"] <6 | data$obs[idx_00,"month"] >9)]
  
  par(mfrow = c(4,3))
  par(oma = c(1,2,1,1), mar = c(2,2,1,1))
  #percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  monsoon_name = c("ALL","Zero flow","Zero flow in monsoon","Zero flow in non-monsoon")
  for( idxi in 1:length(idx_n) ){
    for( j in 1:length(vegname) ){
      plt_idx_daily(data,idx_n[[idxi]],flname,percname,vegname[j],flname_veg)
      if( idxi == 1 ){
        mtext(side = 3, line = 0.8, text = toupper(vegname[j]), cex = 0.8)
      }
      if( j == 1 ){
        mtext(side = 2, line = 2.8, text = monsoon_name[idxi])
      }
      if( idxi == length(idx_n) & j == 2 ){
        mtext(side = 1, line = 2.2, text = "Percentile")
      }
    }
  }
  
}


