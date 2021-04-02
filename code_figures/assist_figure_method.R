library(RColorBrewer)
library(viridis)
plt_method_ill <- function(data,flname){
  obs = data$obs[,flname]
  par( mfrow = c(5,1) )
  par(oma = c(3,5,1.5,2), mar = c(1,4,0,1), xpd = NA ,mgp = c(3,1,0))
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  color = c("orange","purple") #viridis(2) #brewer.pal(3, "Accent")
  
  starty = 1973
  endy = 1974
  idx = which(data$obs$year >= starty & data$obs$year <= endy)
  ## 1. obs
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  plt_tc2color(idx,obs,obs,1,color)
  mtext(side = 2,cex = 1.2,text = ("Runoff"),line = 6.4)
  mtext(side = 2,cex = 1,text = expression(paste("(",mm," ",d^-1,")")) ,line = 4.5)  #expression(paste("( ",m^3,"/",sec," )"))
  legend(x = 10, y = 0.048 ,"(a)",bty = "n",cex = 1.8, y.intersp = 0.18, x.intersp = 0.12,inset = 0, xjust= 0.5, yjust = 0.5)
  legend(x = 500, y = 0.04,legend = c("Positive flow","Zero flow"), col = color[1:2],cex = 1.2,
         bty = "n",lty = 1,lwd = 2,  y.intersp = 1.2, x.intersp = 0.2,inset = 0, xjust= 0.5, yjust = 0.5)
  
  
  ## 2. consecutive 0 days
  cdpm_siyu = get_cumulativedays(obs==0)
  plt_tc2color(idx,obs,cdpm_siyu,0,color)
  mtext(side = 2,"Consecutive",line = 5.7,cex = 1.2)
  mtext(side = 2,"Days",line = 4.2,cex = 1.2)
  legend(x = 10, y = 0.4,"(b)",bty = "n",cex = 1.8, y.intersp = 0.18, x.intersp = 0.12,inset = 0, xjust= 0.5, yjust = -2.8)
  
  ## 3. quantile
  quantname = c("quant_t1c0f1","quant_t0c0f0","quant_t1c1f1")
  plt_8method(data, flname, idx, quantname, colorname)
  mtext(side = 2,"Quantile",line = 4.6,cex = 1.2)
  legend(x = 10, y = 0.045 ,"(c)",bty = "n",cex = 1.8, y.intersp = 0.18, x.intersp = 0.12,inset = 0, xjust= 0.5, yjust = -2.8)
  legend(x = 500, y = 0.8,legend = toupper(substr(percname,6,11)), col = colorname,cex = 1.2,
         bty = "n",lty = c(2,1,1),lwd = 2,  y.intersp = 1.2, x.intersp = 0.2,inset = 0, xjust= 0.5, yjust = 0.5)
  
  ## 4. daily zero fraction
  frname = c("fr_t1c0f1","fr_t0c0f0","fr_t1c1f1")
  plt_8method(data, flname, idx, frname, colorname)
  mtext(side = 2,"Daily",line = 5.7,cex = 1.2)
  mtext(side = 2,"Zero Fraction",line = 4.2,cex = 1.2)
  legend(x = 10, y = 0.045 ,"(d)",bty = "n",cex = 1.8, y.intersp = 0.18, x.intersp = 0.12,inset = 0, xjust= 0.5, yjust = -2.8)
  
  ## 5. percentile
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  plt_8method(data, flname, idx, percname, colorname)
  mtext(side = 2,"Discharge",line = 5.7,cex = 1.2)
  mtext(side = 2,"Percentile",line = 4.2,cex = 1.2)
  axis(side = 1,cex.axis = 1.8,tick = F,at = c(365/2,length(idx)-365/2),labels = as.character(c(starty,endy)) )
  legend(x = 10, y = 0.045 ,"(e)",bty = "n",cex = 1.8, y.intersp = 0.18, x.intersp = 0.12,inset = 0, xjust= 0.5, yjust = -2.8)
  mtext(side = 1,"Time",line = 2.4,cex = 1.3)
  

}



plt_tc2color <- function(idx, q, var, option, color){
  idx_b0 = which(q[idx] > 0)
  idx_00 = which(q[idx] == 0)
  if( option == 1 ){
    loc_b0 = rep(0,length(idx)) #obs
    loc_00 = rep(NA,length(idx))
  }else{
    loc_b0 = rep(NA,length(idx)) #cdpm
    loc_00 = rep(0,length(idx))
  }
  loc_b0[idx_b0] = var[idx[idx_b0]]
  loc_00[idx_00] = var[idx[idx_00]]
  
  ymax = max(var[idx],na.rm = T)*1.05
  plot(var[idx],type = "l",lwd = 1.5, axes = F, ylim = c(-ymax/30,ymax), xlab = "", ylab = "", main = "")
  if( option == 1 ){
    lines(loc_b0, col = color[1]) #obs
    lines(loc_00, col = color[2])
  }else{
    lines(loc_00, col = color[2]) #cdpm
    lines(loc_b0, col = color[1])
  }
  plt_axis(idx,idx_b0)
}


plt_8method <- function(data, flname, idx, varname, colorname){
  q = data$obs[,flname]
  idx_b0 = which(q[idx] > 0)
  linlty = c(2,1,1)
  for (i in length(varname):1){
    td = data[[varname[i]]][idx,flname]
    plot(td, type = "l", ylim = c(-0.05,1.05), col = colorname[i], lwd = 1.5,lty = linlty[i],
         xlab ="", ylab = "", main = "", axes = F)
    par(new = T)
  }
  lines( 1:length(idx), rep( 0.2,length(idx)), xlab = "", ylab = "", type = "l", lty = 2 ,col = 'light gray' ) 
  plt_axis(idx,idx_b0)
}


plt_axis <- function(idx,idx_b0){
  axis(side = 1, at = seq(0,length(idx),365), labels = F)
  axis(side = 1,tick = T,tck = -0.02, at = seq(30,335,30),labels = F)
  axis(side = 1,tick = T,tck = -0.02, at = seq(365,length(idx)-30,30),labels = F)
  axis(side = 2, las = 1, cex.axis = 1.8)
  box()
  left_edge = c(idx_b0[1],idx_b0[which(diff(idx_b0)>1)+1]) 
  right_edge = c(idx_b0[which(diff(idx_b0)>1)],idx_b0[length(idx_b0)])
  usr <- par('usr')
  rect(left_edge[1], usr[3], right_edge[1], usr[4], col=rgb(1,1,0,0.2), border=NA)
  rect(left_edge[2], usr[3], right_edge[2], usr[4], col=rgb(1,1,0,0.2), border=NA)
}
