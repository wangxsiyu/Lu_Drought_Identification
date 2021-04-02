
plt_residue_box <- function(data_residue, flname){
  par(mfrow = c(3,3))
  par(oma = c(1,2,1,1), mar = c(2,2,1,1))
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  resname = paste("res",vegname, sep = "_")
  for (j in 1:length(percname)){
    for (i in 1:length(resname)){
      idx_b = which(data_residue[[resname[i]]][,flname]>0)
      idx_s = which(data_residue[[resname[i]]][,flname]<0)
      tperc_b = data_residue[[percname[j]]][idx_b,flname]
      tperc_s = data_residue[[percname[j]]][idx_s,flname]
      plt_box(tperc_b,tperc_s,colorname[j])
      if( j == 1 ){
        mtext(side = 3, line = 0.5, text = toupper(vegname[i]) )
      }
      if( j == 3 ){
        mtext(side = 1, line = 1, at = c(0.5,1.5), text = c("res>0","res<0"))
      }
      if( i == 1 ){
        mtext(side = 2, line = 2.2, text = percname[j])
      }
    }
  }
}


plt_box <- function(perc_big,perc_small,col_t0c0){
  boxplot(perc_big,xlim = c(0,2),at = 0.5, col = col_t0c0, axes = F)
  boxplot(perc_small,xlim = c(0,2), add = T, at = 1.5, col = col_t0c0, axes = F)
  axis(side = 1, at = c(0.5,1.5), labels = F, tck = -0.02)
  axis(side = 2, las = 1)
  box()
}


plt_residue_perc <- function(data_residue, flname){
  par(mfrow = c(1,3))
  par(oma = c(2,2.5,1,1), mar = c(1.5,2.5,1,1))
  
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  resname = paste("res",vegname, sep = "_")
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  
  bin = seq(0,1,0.1)
  res_percent = NULL
  for( i in 1:length(resname) ){
    for( j in 1:length(percname) ){
      for( bi in 1:(length(bin)-1) ){
        tperc = data_residue[[percname[j]]][,flname]
        idx = which(tperc>=bin[bi] & tperc<bin[bi+1] )
        tres = data_residue[[resname[i]]][idx,flname]
        res_percent[bi] = length(which(tres>0))/length(which(!is.na(tres)))
      }
      plot(res_percent, type= "o", ylim = c(0,1), xlim = c(0,11), axes = F,
           las = 1, xlab = "", ylab = "", main = "", pch = 16, col = colorname[j])
      par(new = T)
    }
    axis(side = 1, at = seq(0.5,10.5,1), labels = bin)
    axis(side = 2, las = 1)
    mtext(side = 3, line = 0.5, text = toupper(vegname[i]))
    box()
    if( i == 1 ){
      mtext(side = 2, line = 3.2, text = " res>0 percentage of each bin")
      legend("topleft", legend = percname, col = colorname,
             pch = 16, lty = 1, bty ="n" )
    }
    par(new = F)
  }
}

