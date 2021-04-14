#######################################
#### Various identification methods
#######################################

# for loop over different flumes
for_allflumes <- function(isquant, d, func_perc, ...){
  doy = get_dayid(d$month, d$day)
  nQ = dim(d)[2] - 3
  percs = d
  for (i in 1:nQ){
    print(sprintf('processing flume %d/%d', i, nQ))
    td = d[,i+3]
    td[which(td <0)] = 0
    te = func_perc(td, doy, ...)
    if( isquant == 0 ){
      if( is.list(te) ){
        percs[,i+3] = te$perc
      }else{
        percs[,i+3] = te
      }
    }else if( isquant == 1 ){
      if( is.list(te) ){
        percs[,i+3] = te$quant
      }else{
        percs[,i+3] = te
      }
    }else if( isquant == 2 ){
      if( is.list(te) ){
        percs[,i+3] = te$fr
      }else{
        percs[,i+3] = te
      }
    }
  }
  return(percs)
}

# Combined method
comb <- function(sf, doy, param_tlm = list(), param_cdpm = list()){
  d_tlm <- tlm(sf, doy, param_tlm)
  d_cdpm <- cdpm(sf, doy, param_cdpm)
  nQ = length(sf)
 
  perc = matrix(NaN, nQ,1)
  perc = d_tlm$perc
  id = which(sf == 0)
  perc[id] = d_cdpm$perc[id]
  
  qt = matrix(NaN, nQ,1)
  qt = d_tlm$quant
  qt[id] = d_cdpm$quant[id]
  
  comb = list(perc = perc, quant = qt, fr = d_cdpm$fr)
  return(comb)
}

# TLM
tlm <- function(sf, doy, param = list()){
  param0 = list(tlm_rgday = 0)
  param = modifyList(param0, param)
  perc = calc_percentile(sf, doy, param$tlm_rgday)
  id = which(sf > 0)
  quant = matrix(NA,length(sf),1)
  quant[id] = calc_percentile(sf[id], doy[id], param$tlm_rgday)
  tlm = list(perc = perc, quant = quant)
  return(tlm)
}

# CDPM
cdpm <- function(sf, doy, param = list()){
  param0 = list(cdpm_rgday = 0, 
                cdpm_option = 'linear', 
                cdpm_ispool = 0,
                tlm_thres = 0.2, 
                tlm_rgday = 0,
                fr_option = 0)
  # cdpm_ispool 
  #     0 = don't pool
  #     1 = use pooled cd
  #     2 = Pieter's weird combination
  param = modifyList(param0, param)
  nQ = length(sf)
  d_tlm = tlm(sf, doy, list(tlm_rgday = param$tlm_rgday))
  cd_0 = calc_0value(sf == 0, param$cdpm_option)
  cd_tlm = calc_0value((sf == 0) | (d_tlm$perc < param$tlm_thres), param$cdpm_option)
  
  quant = switch(param$cdpm_ispool+1,
                 calc_percentile(cd_0, doy, param$cdpm_rgday),
                 calc_percentile(cd_tlm, doy, param$cdpm_rgday),
                 calc_percentile(cd_tlm, doy, param$cdpm_rgday, x_standard = cd_0) # weird Pieter combination
  )
  quant = 1 - quant
  fr = get_fraction(sf, doy, param$fr_option)
  perc = quant * fr
  cdpm = list(perc = perc, fr = fr, quant = quant)
  return(cdpm)
}


#######################################
#### Support functions
#######################################


calc_0value <- function(Q0, param_cdpm = 'linear'){
  # Q0: 1 = 0 runoff, 0 = non-0 runoff
  library(pracma)
  cd = get_cumulativedays(Q0)
  if (param_cdpm == 'triangular'){
    cd_rev = get_cumulativedays(Q0[length(Q0):1])
    cd_rev = cd_rev[length(cd_rev):1]
    # cd = abs((cd - cd_rev)/2)
    te = cbind(cd, cd_rev)
    id = max.col(-te, ties.method ="first")
    cd = arrayfun(function(x){te[x, id[x]]}, 1:length(id))
  }else if(param_cdpm == 'reverse'){
    cd_rev = get_cumulativedays(Q0[length(Q0):1])
    cd_rev = cd_rev[length(cd_rev):1]
    cd = cd_rev
  }
  cd[cd == 0] = NaN
  return(cd)
}

# calculate percentile 
calc_percentile <- function(x, doy, rgday = 0, x_standard = NULL){
  if (is.null(x_standard)){
    x_standard = x
  }
  if (is.infinite(rgday)){
    rgday = 365 # sufficient to cover a year
  }
  nx = length(x)
  perc = matrix(NA, nx, 1)
  nyear = max(365, max(doy,na.rm = T))
  for (di in 1:nyear){ # if doy contains 366, then use 366 days per year
    id = which(doy == di)
    tds = unique(get_daysaround(di, rgday, nyear = nyear))
    txs = x_standard[doy %in% tds]
    if (length(id) > 0){
      perc[id] = get_ecdf(x[id], x_standard = txs)
    }
  }
  perc = perc[,1]
  return(perc)
}

get_daysaround <- function(d, n, nyear = 365){
  ds = (d-n):(d+n)
  while (any(ds <= 0)){
    ds[ds <= 0] = ds[ds <= 0] + nyear
  }
  while (any(ds > nyear)){
    ds[ds > nyear] = ds[ds > nyear] - nyear
  }
  return(ds)
}

# calculate percentiles using ecdf
get_ecdf <- function(x, x_standard = NULL){
  if (is.null(x_standard)){
    x_standard = x
  }
  id = which(!is.na(x_standard))
  if (length(id) > 0){
    tp = ecdf( x_standard[id] )( x )
  } else {
    tp = rep(NA, length(x))
  }
  return(tp)
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


get_fraction <- function(sf, doy, fr_option){
  if(fr_option == 1){
    nQ = length(sf)
    fr = matrix(NaN, nQ,1)
    for (di in 1:max(365, max(doy))){ # if doy contains 366, then use 366 days per year
      id = which(doy == di & !is.na(sf))
      if (length(id) > 0){
        tfr = mean(sf[id] == 0)
        fr[id] = rep(tfr, length(id))
      }
    }
    fr = fr[,1]
  }else{
    id = which(!is.na(sf))
    fr = mean(sf[id] == 0) #fr = length(which(sf==0))/length(sf)
  }
  return(fr)
}


get_cumulativedays<- function(x){
  y = matrix(NaN, length(x),1)
  idnan = which(!is.na(x))
  y[idnan] = 0
  y[idnan[x[idnan]>0]] = 1
  count = 0
  for (i in 1:length(y)){
    if (is.na(y[i]) || y[i] == 0){
      count = 0
    } else {
      count = count + 1
      y[i] = count
    }
  }
  y = y[,1]
  return(y)
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


