calc_dryday <- function(data){
  data_year = calc_annual(data)
  annual = data_year$annual
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
