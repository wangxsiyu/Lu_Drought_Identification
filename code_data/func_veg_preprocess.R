# write.csv(ndvi,file = "/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/NDVI_ext_1912_2020_nosm.csv")
# write.csv(evi,file = "/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/EVI_ext_1912_2020_nosm.csv")
# write.csv(lai,file = "/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/LAI_ext_1912_2020_nosm.csv")

## get_dayid() ===> library(lubridate); 1.yday(ymd(20090228))  2.as.numeric(strftime(ymd(20090228),format = "%j"))

modis_preprocess <- function(obs_d,veg){
  year = veg[,"year"]
  doy = veg[,"doy"]
  ymd = doy2mon_day(year, doy) #change to ymd
  veg = cbind(veg,ymd)
  veg_ext = extend_veg_data(obs_d,veg)
  return(veg_ext)
}

doy2mon_day <- function(year, doy){
  monday = NULL
  for( i in 1:length(year) ){
    if( year[i]%%4 == 0){
      option29 = 1
    }else{
      option29 = 0
    }
    ms = c(31,28 + option29,31,30,31,30,31,31,30,31,30)
    ms = c(0, cumsum(ms),365+option29)
    mon = which(ms >= doy[i])[1]-1
    day = doy[i] - ms[mon]
    mon_day = cbind(mon,day)
    monday = rbind(monday,mon_day)
  }
  colnames(monday) = c("month","day")
  return(monday)
}

extend_veg_data <- function(obs,veg){
  #if(is.null(veg$doy)){
    #doy = get_dayid(veg$month,veg$day)
  #}else{
  #  doy = veg$doy
  #}
  #ymd = c('day','month','year')
  tna = rep(NA,ncol(veg))
  tna = as.data.frame(matrix(tna,1,length(tna)))
  colnames(tna) = colnames(veg)
  out = NULL
  for( i in 1:nrow(obs) ){
    idx_n = which(veg$year == obs$year[i] & veg$month == obs$month[i] & veg$day == obs$day[i])
    if( length(idx_n) == 0 ){
      out = rbind(out,tna)
    }else{
      out = rbind(out,veg[idx_n,])
    }
  }
  out = cbind(out,obs[,c("year","month","day")])
  return(out)
}

# get day of year
get_dayid <- function(M,D, option29 = 0){
  y = matrix(NaN, length(D),1)
  ms = c(31,28 + option29,31,30,31,30,31,31,30,31,30)
  ms = c(0, cumsum(ms))
  for (i in 1:length(D)){
    y[i] = ms[M[i]] + D[i]
  }
  y = y[,1]
  return(y)
}

