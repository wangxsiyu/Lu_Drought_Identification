library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('func_veg_preprocess.R')
vegname = c("ndvi","evi","lai")
obs_d = read.csv(file.path(datadir, 'runoff','runoff_raw.csv'))
for( i in 1:length(vegname) ){
  veg = read.csv( file = file.path(datadir, 'veg_h8v5',paste(vegname[i],'_h8v5.csv',sep = "")) )
  veg_ext = modis_preprocess(obs_d,veg)
  write.csv(veg_ext,file.path(datadir, 'veg_h8v5', paste(vegname[i],'.csv',sep = "")) )
  print(sprintf('veg %d', i))
}






#######
for( i in 1:length(vegname) ){
  veg = read.csv( file = file.path(datadir, 'veg_h8v5',paste(vegname[i],'_h8v5.csv',sep = "")) )
  year = veg[,"year"]
  doy = veg[,"doy"]
  ymd = doy2mon_day(year, doy) #change to ymd
  veg = cbind(veg,ymd)
  write.csv(veg,file.path(datadir, 'veg_h8v5', paste(vegname[i],'_ymd.csv',sep = "")) )
  print(sprintf('veg %d', i))
}