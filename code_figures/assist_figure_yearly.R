plt_flume_daily <- function(data, flname, flname_veg = NULL){
    if (is.null(flname_veg)){
      flname_veg = flname
    }
    par(mfrow = c(3,3))
    par(oma = c(1,2,1,1), mar = c(2,2,1,1))
    percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
    vegname = c("ndvi","evi","lai")
    for (i in 1:3){
      for (j in 1:3){
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