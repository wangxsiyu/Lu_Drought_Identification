plt_8method_multifl_comp <- function(data,flname_all,percname,figname){
  color = c("pink","yellow","purple")
  par(mfrow = c(2,5))
  par(oma = c(1,2,1.5,1), mar = c(2,2,1.5,1))
  for( fli in 1:length(flname_all)){
    daily = get_perc_veg_cor(data, flname_all[fli], percname)
    plt_8method_cor(daily,percname)
    mtext(side = 3, line = 0.3, at = 1, text = flname_all[fli])
    if( fli == 1 ){
      mtext(side = 3, line = 1.5, at = 0.5, text = figname)
      #legend("topleft", legend = toupper(rownames(daily)), col = color, lty = 1, pch = 16,
      #       bty = "n", y.intersp = 0.5, x.intersp = 0.3)
    }
  }
}


plt_8method_1fl_comp <- function(data,data_an,flname,percname){
  color = c("pink","yellow","purple")
  daily = get_perc_veg_cor(data, flname, percname)
  yearly = get_perc_veg_cor(data_an, flname, percname)
  par(mfrow = c(2,1))
  par(oma = c(2,2,1,1), mar = c(2,2,1,1))
  plt_8method_cor(daily,percname)
  mtext(side = 3, line = 0.5, at = 1, text = flname)
  mtext(side = 2, line = 2.5, text = "Daily")
  plt_8method_cor(yearly,percname)
  mtext(side = 2, line = 2.5, text = "Yearly")
  legend("topleft", legend = toupper(rownames(daily)), col = color, lty = 1, pch = 16,
         bty = "n", y.intersp = 0.3, x.intersp = 0.3)
}

plt_8method_cor <- function(re,percname){
  ymin = min(re)
  ymax = max(re)
  color = c("pink","yellow","purple")
  plot(re[1,], type = "o", col = color[1], ylim = c(ymin,ymax), axes = F, pch = 16)
  lines(re[2,], type = "o", col = color[2], pch = 16)
  lines(re[3,], type = "o", col = color[3], pch = 16)
  labname = substr(percname,6,11)
  axis(side = 1, at = 1:8, labels = F,mpg = c(0.5,0.5,0))
  text(x = 1:8,
       y = par("usr")[3]-0.01,
       labels = labname,
       xpd = NA,
       ## Rotate the labels by 35 degrees.
       srt = 30,
       adj = 1,
       cex = 1) 
  axis(side = 2, las = 1)
  box()
}
