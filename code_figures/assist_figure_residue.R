
plt_residue_box <- function(data_residue, flname){
  par(mfrow = c(4,3))
  par(oma = c(1.5,2,1,1), mar = c(1.5,2,1,1))
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  library(effsize)
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  vegname = c("ndvi","evi","lai")
  resname = paste("res",vegname, sep = "_")
  stat = list(ttest_p = matrix(NA,3,3), cohen_d = matrix(NA, 3,3))
  for (j in 1:length(percname)){
    for (i in 1:length(resname)){
      idx_b = which(data_residue[[resname[i]]][,flname]>0)
      idx_s = which(data_residue[[resname[i]]][,flname]<0)
      tperc_b = data_residue[[percname[j]]][idx_b,flname]
      tperc_s = data_residue[[percname[j]]][idx_s,flname]
      stat$ttest_p[j, i]= t.test(tperc_b, tperc_s)$p.value
      stat$cohen_d[j, i]= cohen.d(tperc_b, tperc_s,na.rm = T)$estimate
      plt_box(tperc_b,tperc_s,colorname[j])
      if( j == 1 ){
        mtext(side = 3, line = 0.5, text = toupper(vegname[i]) )
      }
      #if( j == 3 ){
        mtext(side = 1, line = 0.8, at = c(0.5,1.5), text = c("res>0","res<0"))
      #}
      if( i == 1 ){
        mtext(side = 2, line = 2.4, text = percname[j])
      }
    }
  }
  colnames(stat$cohen_d) = resname
  rownames(stat$cohen_d) = percname
  
  for( i in 1:length(vegname) ){
    #plt_bar_cor(stat,i,colorname,percname)
    plt_line_cor(stat,i,colorname,percname)
    if( i == 1 ){
      mtext(side = 2, line = 2.2, text = "Cohen's d")
    }
    box()
  }
  return(stat)
}

plt_box <- function(perc_big,perc_small,col_t0c0){
  boxplot(perc_big,xlim = c(0,2),at = 0.5, col = col_t0c0, axes = F)
  boxplot(perc_small,xlim = c(0,2), add = T, at = 1.5, col = col_t0c0, axes = F)
  axis(side = 1, at = c(0.5,1.5), labels = F, tck = -0.02)
  axis(side = 2, las = 1)
  box()
}

plt_line_cor <- function(stat,i,colorname,percname){
  ymin = min(stat$cohen_d[,i], na.rm = T)
  ymax = max(stat$cohen_d[,i], na.rm = T)
  plot(stat$cohen_d[,i], xlim = c(0.5,3.5), ylim = c(ymin*0.8,ymax*1.1), pch = 15, cex = 1.8,
       col = colorname, xlab = "", ylab = "", main = "", axes = F )
  lines(stat$cohen_d[,i], xlim = c(0.5,3.5), ylim = c(ymin*0.8,ymax*1.1),
       col = "black", xlab = "", ylab = "", main = "", axes = F )
  axis(side = 1, at = 1:3, labels = percname)
  axis(side = 2, las = 1)
}

plt_bar_cor <- function(stat,i,colorname,percname){
    ymax = max(stat$cohen_d[,i], na.rm = T)
    xloc = seq(0.7,3.1,1.2)
    barplot(stat$cohen_d[,i], ylim = c(0,ymax*1.1), col = colorname, xlab = "", ylab = "", main = "", axes = F )
    axis(side = 1, at = xloc, labels = toupper(percname))
    axis(side = 2, las = 1)
    for( jj in 1:3 ){
      if(stat$ttest_p[jj,i]<0.1 & stat$ttest_p[jj,i] >=0.05){
        points(x = xloc[jj],y = ymax*1.05, pch = 8)
      }else if(stat$ttest_p[jj,i]<0.05 & stat$ttest_p[jj,i] >=0.01){
        points(x = xloc[jj]-0.15,y = ymax*1.05, pch = 8)
        points(x = xloc[jj]+0.15,y = ymax*1.05, pch = 8)
      }else if(stat$ttest_p[jj,i]<0.01){
        points(x = xloc[jj],y = ymax*1.05, pch = 8)
        points(x = xloc[jj]-0.15,y = ymax*1.05, pch = 8)
        points(x = xloc[jj]+0.15,y = ymax*1.05, pch = 8)
      }
    }
}

# plt_residue_vio <- function(data_residue, flname){
#   par(mfrow = c(3,3))
#   par(oma = c(1,2,1,1), mar = c(2,2,1,1))
#   col_t1c0f1 = rgb(0,0,1,0.7)   
#   col_t0c0f0 = rgb(1,0.1,0,0.7)  
#   col_t1c1f1 = rgb(0,1,0,0.7)
#   colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
#   library(effsize)
#   percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
#   vegname = c("ndvi","evi","lai")
#   resname = paste("res",vegname, sep = "_")
#   stat = list(ttest_p = matrix(NA, 3,3), cohen_d = matrix(NA, 3,3))
#   pp = matrix(list(), 3,3)
#   for (j in 1:length(percname)){
#     for (i in 1:length(resname)){
#       idx_b = which(data_residue[[resname[i]]][,flname]>0)
#       idx_s = which(data_residue[[resname[i]]][,flname]<0)
#       tperc_b = data_residue[[percname[j]]][idx_b,flname]
#       tperc_s = data_residue[[percname[j]]][idx_s,flname]
#       stat$ttest_p[j, i]= t.test(tperc_b, tperc_s)$p.value
#       stat$cohen_d[j, i]= cohen.d(tperc_b, tperc_s)$estimate
#       library(ggpubr)
#       pp[[i,j]] = plt_violin(tperc_b,tperc_s)  # violin plot
#     }
#   }
#   library(gridExtra)
#   grid.arrange(pp[[1,1]], pp[[1,2]],pp[[1,3]],pp[[2,1]],pp[[2,2]],pp[[2,3]],
#                pp[[3,1]],pp[[3,2]],pp[[3,3]],
#                ncol = 3, nrow = 3)
#   return(stat)
# }


# plt_violin <- function(perc_big,perc_small){
#   
#   library(ggplot2)
#   
#   # create a dataset
#   data <- data.frame(
#     name=c( rep('res +',length(perc_big)), rep('res -', length(perc_small))  ),
#     value=c( perc_big, perc_small )
#   )
#   # Most basic violin chart
#   p <- ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
#     geom_violin()
#   return(p)
# }

plt_violinplot <- function(perc_big,perc_small){
  library(vioplot)
  vioplot(perc_big,perc_small, cex.lab = 10,las = 1,mgp = c(0.5,0.5,0),cex.axis = 1.3,
          col = c("tomato2", "orange"),
          rectCol=c("palevioletred", "peachpuff"),
          lineCol=c("red4", "orangered"),
          border=c("red4", "orangered"),
          colMed = "white",pchMed = 19,drawRect = T,names = c("res>0","res<0"))
}


plt_dist <- function(perc_big,perc_small,col_t0c0){
  plot(density(perc_big),xlim = c(0,1) ,col = col_t0c0, axes = F)
  par(new = T)
  plot(density(perc_small),xlim = c(0,1), col = col_t0c0, axes = F)
  par(new = F)
  #axis(side = 1, at = c(0.5,1.5), labels = F, tck = -0.02)
  #axis(side = 2, las = 1)
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

