calc_residue <- function(data){
  #re = list()
  # compute residual
  data$res_ndvi = get_residual(data$ndvi)
  data$res_evi = get_residual(data$evi)
  data$res_lai = get_residual(data$lai)
  return(data)
}

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

get_residual <- function(x){
  doy = get_dayid(x$month,x$day)
  ymd = c('year','month','day')
  datacol = setdiff(names(x),ymd)
  re = NULL
  for( fli in 1:length(datacol)){
    tav = calc_annualaverage(doy,x[,datacol[fli]])
    re = cbind(re,tav)
  }
  re = cbind(x[,ymd],re)
  colnames(re) = colnames(x)
  return(re)
}

calc_annualaverage <- function(doy,td){
  per_mean = NULL
  day365 = max(365, max(doy))
  # take mean
  for( dayi in 1: day365){
    idx_d = which(doy == dayi)
    per_mean[dayi] = mean(td[idx_d],na.rm = T)
  }
  id = which(!is.na(per_mean))
  # fit using spline
  func  = smooth.spline(x = id, y = per_mean[id])
  yy = predict(func,1:day365)$y
  # calc residue
  for( dayi in 1:day365 ){
    idx_d = which(doy == dayi)
    td[idx_d] = (td[idx_d] - yy[dayi])
  }
  return(td)
}