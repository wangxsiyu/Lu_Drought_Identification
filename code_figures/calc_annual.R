calc_annual <- function(data){
  result = list()
  # compute annual (perc/neg)
  data_name = names(data)
  annual = list()
  for( di in 1:length(data_name) ){
    td = data[[data_name[di]]]
    annual[[data_name[di]]] = get_year(td)
  }

  result$annual = annual
  return(result)
}

calc_dryday <- function(data){
  annual = calc_annual(data)$annual
  ## dryday x perc x 8
  for (t in 0:1){
    for (c in 0:1){
      for (f in 0:1){
        percname = sprintf("perc_t%dc%df%d", t, c, f)
        drydayname = sprintf("dryday_t%dc%df%d", t, c, f)
        annual[[drydayname]] = get_year_dry(data[[percname]])
      }
    }
  }
  return(annual)
}

calc_seasonal <- function(data){
  result = list()
  # compute seasonal (perc/neg)
  data_name = names(data)
  seasonname = c("mons","postmons","winter","premons")
  seasonal = list()
  for( di in 1:length(data_name) ){
    td = data[[data_name[di]]]
    for( sei in 1:4 ){
      sename = paste(data_name[di],"_",seasonname[sei], sep = "")
      seasonal[[sename]] = get_seasonal(td)[[sei]]
    }
  }
  
  result$seasonal = seasonal
  return(result)
}


get_seasonal <- function(td){
  years = unique(td$year)
  out = matrix(list(matrix(NA,length(years),ncol(td))),1,4)
  for( yi in 1:length(years)){
    id_mons = which(td$year == years[yi] & td$month >= 6 & td$month <= 9)
    id_postmons = which(td$year == years[yi] & td$month >= 10 & td$month <= 12)
    id_winter = which(td$year == years[yi] & td$month >= 1 & td$month <= 3)
    id_premons = which(td$year == years[yi] & td$month >= 4 & td$month <= 5)
    out[[1]][yi,] = colMeans(td[id_mons,],na.rm = T)
    out[[2]][yi,] = colMeans(td[id_postmons,],na.rm = T)
    out[[3]][yi,] = colMeans(td[id_winter,],na.rm = T)
    out[[4]][yi,] = colMeans(td[id_premons,],na.rm = T)
  }
  colnames(out[[1]]) = colnames(out[[2]]) = colnames(out[[3]]) = colnames(out[[4]]) = colnames(td)
  data_col = setdiff(colnames(out[[1]]),c("month","day","X"))
  for( i in 1:ncol(out)){
    out[[i]] = out[[i]][,data_col]
  }
  return(out)
}

get_year <- function(td){
  years = unique(td$year)
  out = matrix(NA,length(years),ncol(td))
  for( yi in 1:length(years)){
    id = which(td$year == years[yi])
    out[yi,] = colMeans(td[id,],na.rm = T)
  }
  colnames(out) = colnames(td)
  data_col = setdiff(colnames(out),c("month","day"))
  out = out[,data_col]
  return(out)
}

get_year_dry <- function(td){
  years = unique(td[,"year"])
  ndata = setdiff(colnames(td),c("year","month","day","X"))
  dry_y = matrix(NA,length(years),length(ndata)+1)
  for( yi in 1:length(years)){
    id = which(td[,"year"] == years[yi])
    for( fli in 1:length(ndata)){
      if( length(which(is.na(td[id,ndata[fli]])))>360 ){
        dry_y[yi,fli+1] = NA
      }else{
        dry_y[yi,fli+1] = length(which(td[id,ndata[fli]]<0.2))
      }
    }
  }
  colnames(dry_y) = c("year",ndata)
  dry_y[,1] = years
  return(dry_y)
}



