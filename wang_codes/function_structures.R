combine <- function(d1, d2){
  flumes = names(d2)
  for (fi in 1:length(flumes)){
    d1[[flumes[fi]]] = append(d1[[flumes[fi]]], d2[[flumes[fi]]])
  }
  return(d1)
}

data2flume <- function(data){
  flumes = setdiff(names(data$obs), c("year","month","day")) 
  out = list()
  for (fli in 1:length(flumes)){
    flname = flumes[fli]
    out[[flname]] = getdata_byflume(data, flname)
  }
  return(out)
}

flume2data <- function(d, vartokeep = NULL){
  flumes = names(d)
  varnames = names(d[[flumes[1]]])
  varnames = setdiff(varnames, vartokeep)
  out = list()
  for (vi in 1:length(varnames)){
    
    if (is.null(vartokeep)){
      out1 = NULL;
    }else{
      out1 = d[[flumes[1]]][, vartokeep]
    }
    for (fli in 1:length(flumes)){
      out1 = cbind(out1, d[[flumes[fli]]][, varnames[vi]])
    }
    out1 = as.data.frame(out1)
    names(out1) = c(vartokeep,flumes)
    
    out[[varnames[vi]]] = out1
  }
  if (length(varnames) == 1){
    out = out1;
  }
  return(out)
}

flumevegperc2data <- function(d, vartokeep = NULL){
  flumes = names(d)
  tabnames = names(d[[flumes[1]]])
  varnames = names(d[[flumes[1]]][[tabnames[1]]])
  varnames = setdiff(varnames, vartokeep)
  out = list()
  for (vi in 1:length(varnames)){
    out1 = list()
    for (ti in 1:length(tabnames)){
      
      out2 = NULL;
      if (!is.null(vartokeep)){
        for (ii in 1:length(vartokeep)){
          out2 = cbind(out2, d[[flumes[1]]][[tabnames[ti]]][[vartokeep[ii]]]) 
        }
      }
      for (fli in 1:length(flumes)){
        out2 = cbind(out2, d[[flumes[fli]]][[tabnames[ti]]][[varnames[vi]]])
      }
      out2 = as.data.frame(out2)
      names(out2) = c(vartokeep,flumes)
      out1[[tabnames[ti]]] = out2
    }
    out[[varnames[vi]]] = out1
  }
  return(out)
}

getdata_byflume <- function(data, flname){
  datanames = names(data)
  ymd = c("year","month","day")
  data_fl = data$obs[,ymd]
  for (di in 1:length(datanames)){
    td = data[[datanames[di]]]
    data_fl = cbind(data_fl, td[, flname])
  }
  names(data_fl) = c(ymd, datanames)
  return(data_fl)
}

analyzebyflumes <- function(flumes, func){
  flnames = names(flumes)
  result_flumes = list()
  for (fi in 1:length(flnames)){
    print(sprintf("flume %d/%d: %s", fi, length(flnames), flnames[fi]))
    result_flume = list()
    dfl = flumes[[flnames[fi]]]
    dfl$doy = get_dayid(dfl$month, dfl$day)
    result_flumes[[flnames[fi]]] = func(dfl)
  }
  return(result_flumes)
}

analyzebyflumes_vegperc <- function(flumes, func, isreduce = T){
  flnames = names(flumes)
  result_flumes = list()
  for (fi in 1:length(flnames)){
    print(sprintf("flume %d/%d: %s", fi, length(flnames), flnames[fi]))
    result_flume = list()
    dfl = flumes[[flnames[fi]]]
    ymd = c("year", "month", "day")
    vegnames = c("ndvi", "evi", "lai")
    obsnames = c("obs")
    percnames = setdiff(names(dfl), c(ymd, vegnames,obsnames))
    # analyze each vegetation vs percentile combination
    for (vi in 1:length(vegnames)){
      print(sprintf("vegetation %d/%d: %s", vi, length(vegnames), vegnames[vi]))
      for (pi in 1:length(percnames)){
        tveg = dfl[, vegnames[vi]]
        tperc = dfl[, percnames[pi]]
        td = data.frame(veg = tveg, perc = tperc, obs = dfl[, obsnames])
        td = cbind(dfl[, ymd], td)
        # td$fr = get_fr(td$doy, td$obs)
        if (isreduce){
          id_valid = which(colMeans(t(is.na(td))) == 0)
          td = td[id_valid,]
        }
        td$doy = get_dayid(td$month, td$day)
        tnm = sprintf("vegperc_%s_%s", vegnames[vi], percnames[pi])
        result_flume[[tnm]] = func(td)
      }
    }
    result_flumes[[flnames[fi]]] = result_flume
  }
  return(result_flumes)
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
