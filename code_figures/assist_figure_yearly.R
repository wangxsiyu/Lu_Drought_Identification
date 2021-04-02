plt_flume_daily <- function(data, flname,percname, flname_veg = NULL){
  if (is.null(flname_veg)){
    flname_veg = flname
  }
  par(mfrow = c(3,3))
  par(oma = c(1,2,1,1), mar = c(1.5,2,1,1))
  #percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  
  for (i in 1:length(vegname)){
    for (j in 1:length(percname)){
      tperc = data[[percname[j]]][,flname]
      tveg = data[[vegname[i]]][, flname_veg]
      plt_trendline(tperc, tveg, colorname[j])
      if (j == 1){
        mtext(side = 2, line = 2.5, text = toupper(vegname[i]), cex = 0.8)
      }
      if (i == 1){
        mtext(side = 3, line = 0.1, text = paste(percname[j], flname, sep = ","), cex = 0.8)
      }
    }
  }
}

plt_trendline <- function(a,b,colorname){
  require(basicTrendline)
  idx = !is.na(a) & !is.na(b)
  a = a[idx]
  b = b[idx]
  trendline(x = a,y = b, model = "line2P",summary = F,show.Rsquare = T, show.pvalue = T, show.equation = F, 
            pch = 19, xlab = "",ylab = "", col = colorname, linecolor = "black",eDigit = 2, eSize = 1.4)
}
