ma <- function(x,M=30){filter(x,rep(1/M,M),sides=1)}

preprocess <- function(d, area = NULL){
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
