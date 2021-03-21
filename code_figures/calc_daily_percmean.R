calc_cor_percmean <- function(data, flname){
  tn = names(data)
  d = NULL
  for (ti in 1:length(tn)){
    d[[tn[ti]]] = data[[tn[ti]]][, flname]
  }
  d = as.data.frame(d)
  d1 = d[,2:4]
  d2 = d[,c(1,5:dim(d)[2])]
  recor = matrix(list(),1,8)
  for (i in 1:12){
    td2 = preprocess_average(d2, i-10)
    recor[[i]] = cor(td2,d1, use = "pairwise")
  }
}
preprocess_average <- function(d, nav = 1){
  sf = d
  for(sfi in 1:ncol(sf)){
    Q_ma = mam(sf[,sfi], nav)
    sf[,sfi] = Q_ma
  }
  return(sf)
}

mam <- function(x,nav){
if (nav > 0){
  for (i in length(x):(nav+1)){
    x[i] = x[(i-nav)]#mean(x[(i-nav):i], na.rm = T)
  }}else if (nav <0){
    for (i in 1:(length(x)+nav)){
      x[i] = x[(i-nav)]#mean(x[(i-nav):i], na.rm = T)
    }
  }
  return(x)
}
