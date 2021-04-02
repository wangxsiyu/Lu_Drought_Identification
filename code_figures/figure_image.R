library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
data = read.csv("../data/runoff/runoff.csv")
{
library(fields)
### image
flname = "f001"
starty = 1957
endy = 2020

#### obs
obs = data$obs
idxx = which(obs$year >= starty & obs$year <= endy)
obs = obs[idxx,]
obs_q = obs[,flname]

idx_p = which(obs_q>0)
idx_0 = which(obs_q == 0)
obs_pos = obs_q[idx_p]
obs_q[idx_p] = obs_pos/max(obs_pos,na.rm = T)  #>0 normalize
cd_0 = calc_0value(obs_q == 0)
nor_cd = cd_0/max(cd_0,na.rm = T)
obs_q[idx_0] = -nor_cd[idx_0] #==0 cdpm normalize

doy = get_dayid(obs$month,obs$day)
year = obs$year
zlime = c(min(obs_q,na.rm = T), max(obs_q,na.rm = T)) #c(0,max(obs[,flname],na.rm = T))
colorimage = c(colorRampPalette(c("red","yellow"))(50),colorRampPalette(c("green","blue","black","black","black"))(50))
               #colorRampPalette(c("blue","black"))(40))
param = list(method = "Runoff", zlime = zlime, color = colorimage)

tab_obs = transmat(doy, year, obs_q)
}
#### perc
perc = read.csv("../data/perc/perc_T0_C0_fr0_1912_2020.csv")
idxx = which(perc$year >= starty & perc$year <= endy)
perc = perc[idxx,]
percc = perc[,flname]
doy_p = get_dayid(perc$month,perc$day,option29 = 0)
year_p = perc$year
colorimage2 = tim.colors(100)
param2 = list(method = "T0C0F0", zlime = c(0,1), color = colorimage2)
tab_perc = transmat(doy_p, year_p, percc)

####################  plot  ########################
par(plt = c(0.07,0.3, 0.1,0.9), mgp = c(1,0.5,0) )
image(x=tab_obs$x, y = tab_obs$y, z = tab_obs$z, xlab = "",ylab = "", main = param$method, zlim = param$zlime,
           col = param$color)

par(new = T, plt = c(0.015,0.025, 0.2,0.8) )
z = array(1:100, dim = c(1,100) )
image( 1,1:100, z, col = colorimage, axes = FALSE, xlab = "", ylab = "" )
a = seq(0,max(cd_0,na.rm = T),length.out = 6)
b = seq(min(obs_pos),max(obs_pos),length.out = 6)
axis( side = 4, at = 50-c(0,100,200,300)/max(cd_0,na.rm = T)*50, tck = -0.2, labels = F )
mtext( side = 4, at = 50-c(0,100,200,300)/max(cd_0,na.rm = T)*50, line = 0.3, 
       text = as.character(c(0,100,200,300)), las = 1)
axis( side = 4, at = c(0,0.1,0.2,0.3,0.4)/max(obs_pos)*50+50, tck = -0.2, labels = F )
mtext( side = 4, at = c(0,0.1,0.2,0.3,0.4)/max(obs_pos)*50+50, line = 0.3, 
       text = as.character(c(0,0.1,0.2,0.3,0.4)), las = 1)
box()
mtext(side = 2, at = 20, line = 0.1, text = "Cumulative dry days")
mtext(side = 2, at = 70, line = 0.1, text = "Runoff")

####### ecdf
par(new = T, plt = c(0.335, 0.7, 0.1,0.9) )
source("./figure_ecdf.R")

####### perc
par(new = T, plt = c(0.71,0.72, 0.2,0.8) )
z = array(1:100, dim = c(1,100) )
image( 1,1:100, z, col = colorimage, axes = FALSE, xlab = "", ylab = "" )
a = seq(0,max(cd_0,na.rm = T),length.out = 6)
b = seq(min(obs_pos),max(obs_pos),length.out = 6)
axis( side = 4, at = 50-c(0,100,200,300)/max(cd_0,na.rm = T)*50, tck = -0.2, labels = F )
mtext( side = 4, at = 50-c(0,100,200,300)/max(cd_0,na.rm = T)*50, line = 0.3, 
       text = as.character(c(0,100,200,300)), las = 1)
axis( side = 4, at = c(0,0.1,0.2,0.3,0.4)/max(obs_pos)*50+50, tck = -0.2, labels = F )
mtext( side = 4, at = c(0,0.1,0.2,0.3,0.4)/max(obs_pos)*50+50, line = 0.3, 
       text = as.character(c(0,0.1,0.2,0.3,0.4)), las = 1)
box()

par(new = T, plt = c(0.76,0.99, 0.1,0.9) )
image(x=tab_perc$x, y = tab_perc$y, z = tab_perc$z, xlab = "",ylab = "", main = param2$method, zlim = param2$zlime,
           col = param2$color)




