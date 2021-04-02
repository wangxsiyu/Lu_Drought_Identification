calc_cor_percmean <- function(data, flname){
  tn = names(data)
  d = NULL
  for (ti in 1:length(tn)){
    d[[tn[ti]]] = data[[tn[ti]]][, flname]
  }
  d = as.data.frame(d)
  d1 = d[,2:4] #veg
  d2 = d[,c(1,5:dim(d)[2])] #obs,perc
  recor = matrix(list(),1,41)
  for (i in 1:41){
    td2 = preprocess_average(d2, i-21) 
    #td2 = preprocess_average(d2, 0) 
    recor[[i]] = cor(td2,d1, use = "pairwise")
  }
  rep = matrix(list(matrix(NA,ncol(td2),ncol(d1))),1,41)
  for(lagi in 1:41){
    for( i in 1:ncol(td2) ){
      for( j in 1:ncol(d1) ){
        rep[[lagi]][i,j] = cor.test(td2[,i],d1[,j])$p.value
      }
    }
    colnames(rep[[lagi]]) = colnames(d1)
    rownames(rep[[lagi]]) = colnames(td2)
  }
  
  return(list(cor = recor,p = rep))
}

# calc_cor_percmean <- function(data, flname){
#   tn = names(data)
#   d = NULL
#   for (ti in 1:length(tn)){
#     d[[tn[ti]]] = data[[tn[ti]]][, flname]
#   }
#   d = as.data.frame(d)
#   d1 = d[,2:4]
#   d2 = d[,c(1,5:dim(d)[2])]
#   recor = matrix(list(),1,8)
#   recor[[i]] = cor(d2,d1, use = "pairwise")
# }

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
      x[i] = x[(i-nav)]
      #x[i] = mean(x[(i-nav):i], na.rm = T)
    }
    x[1:nav] = NA
  }else if (nav <0){
      for (i in 1:(length(x)+nav)){
        x[i] = x[(i-nav)]
        #x[i] = mean(x[(i-nav):i], na.rm = T)
      }
    x[(length(x)+nav+1):length(x)] = NA
  }
  return(x)
}
