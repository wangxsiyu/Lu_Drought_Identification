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