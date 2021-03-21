loaddata <- function(datadir){
  data = list()
  # load streamflow
  data$obs = read.csv(file.path(datadir, "runoff", "runoff.csv"))
  # read vegetation
  data$ndvi = read.csv(file.path(datadir, "vegetation",  "ndvi.csv"))
  data$evi = read.csv(file.path(datadir, "vegetation", "evi.csv"))
  data$lai = read.csv(file.path(datadir, "vegetation", "lai.csv"))
  ## perc x 8
  for (t in 0:1){
    for (c in 0:1){
      for (f in 0:1){
         filename = sprintf("perc_T%d_C%d_fr%d_1912_2020.csv", t,c,f)
         varname = sprintf("perc_t%dc%df%d", t, c, f)
         data[[varname]] = read.csv(file.path(datadir, "perc", filename))
      }
    }
  }
  return(data)
}

ma <- function(x,M=30){filter(x,rep(1/M,M),sides=1)}

preprocess <- function(d, area = NULL){
  doy = get_dayid(d$month, d$day)
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
