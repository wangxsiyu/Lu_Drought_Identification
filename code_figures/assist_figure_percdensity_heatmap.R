library(fields)
library(plotrix)
plt_perc_hist_heatmap <- function(data,percname,flname){
  td = data$obs
  doy = get_dayid(td$month,td$day)
  day365 = max(365, max(doy))
  tperc = data[[percname]][,flname]
  den = matrix(list(),1,day365)
  chose_20point = matrix(NA,100,day365)
  for(dayi in 1:day365){
    id = which(doy == dayi)
    den[[dayi]] = density(tperc[id],na.rm = T,n=1000)
    id_den = seq(1,1000,length.out = 100)
    chose_20point[,dayi] = den[[dayi]]$y[id_den]
    #plot(density(tperc[id],na.rm = T), xlab = "", ylab = "", main = "", 
    #     col = tim.colors(365)[dayi], xlim = c(0,1), ylim = c(0,2.5))
    #par(new = T)
  }
  
  #par(mfrow = c(1,1))
  #par(oma = c(1,1,1,0.5), mar = c(1,1,1,0.5))
  image.plot(chose_20point, las = 1, col = c("white",tim.colors(64)))
  pname = toupper(substr(percname,6,11))
  fname = str_remove(flname,"x")
  mtext(side = 3, line = 0.5, text = paste(pname,fname, sep = "  fl_"))
  
  return(list(heatmap = chose_20point, density = den))
}



