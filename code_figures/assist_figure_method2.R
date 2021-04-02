plt_method2 <- function(data,data_year, flname, area){
  Q <- data$obs[,c("year","month","day",flname)]
  colnames(Q) = c("Year","Month","Day","Q")
  doy = get_dayid(Q$Month,Q$Day) 
  Q_area = Q$Q #((Q$Q*3600*24 / area[1]))*10^(-3)  ## mm/d 
  Q = cbind(Q, doy, data.frame(Q_area))
  
  annual_dry = NULL
  dryname = c("dryday_t1c0f1","dryday_t0c0f0","dryday_t1c1f1")
  for( i in 1:length(dryname) ){
    annual_dry = rbind(annual_dry,data_year[[dryname[i]]][,flname])
  }
  
#### plot
  starty = 2000
  endy = 2011
  Yr = unique(data$obs[,"year"])
  idx = which((Q$Year >= starty)& (Q$Year <= endy))
  Time = 1:dim(Q[idx,])[1]
  ylimyr = c(-0.01,max(Q$Q_area[idx],na.rm = T)*1.1)
  ylegend = 0.17
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  mename = toupper(substr(percname,6,11))
  pname = c("(b)","(c)","(d)")
  
  par(mfrow = c(5,1))
  par(oma = c(2.5,4,1.5,4), mar = c(1,3,0,1), xpd = NA ,mgp = c(3,1,0))
  # Panel 1
  plot(Q$Q_area[idx],type = "l", ylim = ylimyr ,axes = F,xlab ="",ylab ="")
  axis( side = 1, at = c(which(Q$doy[idx] == 1),length(Q$doy[idx])),labels = F,tck = -0.03)
  axis(side = 2,las = 1,cex.axis = 1.8)
  box()
  mtext(side = 2,cex = 1.2,text = ("Runoff"),line = 5)
  mtext(side = 2,cex = 1,text = expression(paste("(",mm," ",d^-1,")")),line = 3.5)
  legend(x = 0, y = 0.15 ,"(a)",bty = "n",cex = 1.8, y.intersp = 0.18, x.intersp = 0.12,inset = 0, xjust= 0.5, yjust = 0.5)
  
  # Panel 2-4: 3 methods
  for( i in 1:length(percname) ){
    idx_dry = data[[percname[i]]][,flname] < 0.2
    td = data[[percname[i]]][idx,flname]
    plt_perc(idx, td, Q, idx_dry, mename[i], pname[i], colorname[i])
    if( i == 1 ){
    legend(2000, ylegend, legend = "Drought threshold",lty = 2, col = "grey", text.col = "black", bty = "n", cex = 1.5)
    }
  }
  

  # Panel 5
  annual_idx = annual_dry[,(starty-Yr[1]+1):(endy-Yr[1]+1) ]
  barplot( annual_idx,ylim = c(0, ceiling(max(annual_idx)+50)),
           cex.names = 2,axes = F,axisnames = F,xlab = NULL,ylab = NULL,beside = T,col = colorname) 
  axis(1, at = seq(2.5,4*(endy+1-starty)-1,8), labels = seq(starty,endy,2), cex.axis = 1.8, las = 1, mgp = c(0.8,0.8,0),tck = 0)   
  axis(1, at = seq(6.5,4*(endy+1-starty)-1,8)[1:6], labels = seq(starty+1,endy,2) , cex.axis = 1.8, las = 1, mgp = c(0.8,0.8,0),tck = 0)  
  axis(1, at = seq(0.5,4*(endy+1-starty)+0.5,4), labels = F, tck = -0.03)   #seq(from,to,by)    seq(range(firstday)[1],range(firstday)[2],365*2)
  
  axis( side = 2, at = seq(0,ceiling(max(annual_idx))+50,50),labels = seq(0,ceiling(max(annual_idx))+50,50),cex.axis = 2, col = 'black', las = 1, mgp = c(0.7,0.7,0), tck = -0.02 )   
  mtext( side = 1, cex = 1.2, text = "Time(DOY)", line = 2.3)
  mtext( side = 2, cex = 1.2, text = "Duration", line = 4)
  box()
  #legend(2,max(annual_idx)+50, col = colorname, pch = 15, legend = mname,
  #       y.intersp = 1, x.intersp = 0.3, bty = "n",cex = 1.5)
  legend(x = 1, y = max(annual_idx) ,"(e)",bty = "n",cex = 1.8, y.intersp = 0.18, x.intersp = 0.12,inset = 0, xjust= 0.5, yjust = 0.5)
  
}


plt_perc <- function(idx, td, Q, idx_dry, mename, pname, color){
  Time = 1:dim(Q[idx,])[1]
  ptcex = 0.1 
  ptcex2 = 0.8
  ylegend = 0.17
  ylimyr = c(-0.01,max(Q$Q_area[idx],na.rm = T)*1.1)
  
  plot(td,ylim = c(0,1), type = "l",col = "grey",axes = F,xlab ="",ylab ="")
  axis(side = 4,las = 1,cex.axis = 1.8,col = "grey",col.axis = "grey")
  lines(Time, rep( 0.2,length(Time)), xlab = "", ylab = "", type = "l", lty=2 ,col = 'grey' ) 
  par(new = T)
  plot(Q$Q_area[idx],type = "l",ylim = ylimyr ,axes = F,xlab ="",ylab ="")
  points(x = which(idx_dry[idx]), y = rep(-0.01,length(which(idx_dry[idx]))),col = color,pch = 15,cex = c(ptcex, ptcex2)) #y = rf[which(new_percent[,4]<=0.2)]
  
  axis(side = 1, at = c(which(Q$doy[idx] == 1),length(Q$doy[idx])),labels = F,tck = -0.03)
  axis(side = 2,las = 1,cex.axis = 1.8)
  box()
  mtext(side = 2,cex = 1.2,text = ("Mean runoff"),line = 5)
  mtext(side = 2,cex = 1,text = expression(paste("(",mm," ",d^-1,")")),line = 3.5)
  mtext(side = 4,cex = 1.2,text = ("Percentile"),line = 3.5,col = "grey")
  legend(100, ylegend, legend = mename, x.intersp = 0.3, pch = 15, col = color, bty = "n", cex = 1.5)
  legend(x = 0, y = 0.15 ,pname,bty = "n",cex = 1.8, y.intersp = 0.18, x.intersp = 0.1,inset = 0, xjust= 0.5, yjust = 0.5)
  
  
}
