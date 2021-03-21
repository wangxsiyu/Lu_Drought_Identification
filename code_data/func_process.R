preprocess_average <- function(d, nav = 1){
  ymd = c('year','month','day')
  sf = d[, setdiff(names(d),ymd)]
  sf_afma = sf
  for(sfi in 1:ncol(sf)){
    Q_ma = mam(sf[,sfi], nav)
    sf_afma[,sfi] = Q_ma
  }
  sf_ma = cbind(d[,ymd],sf_afma)
  return(sf_ma)
}

mam <- function(x,nav){
  for (i in length(x):(nav+1)){
     x[i] = mean(x[(i-nav):i], na.rm = T)
  }
  return(x)
}

ma <- function(x,M=30){filter(x,rep(1/M,M),sides=1)}
preprocess <- function(d, area = NULL){
  # doy = get_dayid(d$month, d$day)
  ymd = c('year','month','day')
  sf = d[, setdiff(names(d),ymd)]
  sf_afma = sf
  for(sfi in 1:ncol(sf)){
    Q_ma = ma(sf[,sfi])
    if (!is.null(area)){
      # delete - dataarea <- read.csv(paste(runoff_obs_direc,"area.csv",sep = "/"),header=T,stringsAsFactors=FALSE, sep ='\t')
      # delete - area = dataarea[which(dataarea[,4] == 1),3]*0.00404686 #km^2
      Q_ma = ((Q_ma*3600*24 / area[1]))*10^(-3)  ## mm/d
    }
    sf_afma[,sfi] = Q_ma
  }
  sf_ma = cbind(d[,ymd],sf_afma)
  return(sf_ma)
}
# get_dayid <- function(M,D, option29 = 0){
#   y = matrix(NaN, length(D),1)
#   ms = c(31,28 + option29,31,30,31,30,31,31,30,31,30)
#   ms = c(0, cumsum(ms))
#   for (i in 1:length(D)){
#     y[i] = ms[M[i]] + D[i]
#   }
#   y = y[,1]
#   return(y)
# }