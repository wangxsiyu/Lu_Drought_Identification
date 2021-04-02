library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
##################################
d = read.csv("../data/runoff/runoff.csv")
doy = get_dayid(d$month, d$day)
param = list()
param$cdpm_option = 'linear'
param$fr_option = 0
param$tlm_rgday = 365

flname = "f001"
td = d[,flname]
td[which(td <0)] = 0
sf = td

## cdpm
cd_0 = calc_0value(sf == 0, param$cdpm_option)
x_standard1 = cd_0

txs1 = x_standard1
tmin = min(txs1,na.rm = T)
tmax = max(txs1,na.rm = T)
t1 = seq(0.5,tmax+0.5,1)
quant_ori1 = get_ecdf(t1, txs1)
quant = 1 - quant_ori1
fr = get_fraction(sf, doy, param$fr_option)
perc1 = quant * fr


## tlm
id = which(sf > 0)
x_standard2 = sf[id]

txs2 = x_standard2
tmin = min(txs2,na.rm = T)
tmax = max(txs2,na.rm = T)
t2 = seq(0,0.42,0.001)
quant_ori = get_ecdf(t2, txs2)
perc2 = get_ecdf(t2, sf)

############
### 1. ecdf
{
  t11 = 0:(length(t1)-1)
  plot(x = -t11, y = quant, type = "l", xlim = c(-length(t11),length(t11)), axes  = F, xlab = "",ylab = "", main = "")
  lines(x = -t11, y = rep(fr,length(t11)), col = "darkgrey")
  text(x = -t11[length(t11)]+50, y = fr+0.03, labels = "Fraction")
  lines(x = -t11, y = perc1, type = "l", col = "orange")
  
  
  t22 = 0:(length(t2)-1)
  lines(x = t22, y = quant_ori, type = "l")
  lines(x = t22, y = rep(fr,length(t22)), col = "darkgrey")
  lines(x = t22, y = perc2, type = "l", col = "orange")
  
  abline(v = 0, lty = 2)
  idx1 = seq(length(t11),1,-50)
  idx2 = seq(1,length(t22),50)
  axis(side = 1, at = -t11[idx1]+0.5, labels = t1[idx1]-0.5, col.ticks = "red", col.axis = "red")
  axis(side = 1, at = t22[idx2], labels = t2[idx2], col.ticks = "blue", col.axis = "blue")
  axis(side = 1, at = 0, labels = 0, col.ticks = "black", col.axis = "black")
  axis(side = 2, las = 1)
  box()
  
  legend(100,0.2, legend = c("Quantile","Percentile","Fraction"), col = c("black","orange","darkgrey"), 
         lty = 1, bty = "n", y.intersp = 0.8, x.intersp = 0.3)
}




