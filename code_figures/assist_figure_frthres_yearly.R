source('assist_figure_fr0_doy.R')
calc_frthreshold_yearly <- function(data,frthres){
  result = list()
  # compute annual (perc/neg)
  data_name = names(data)
  annual = list()
  for( di in 1:length(data_name) ){
    td = data[[data_name[di]]]
    annual[[data_name[di]]] = get_frthreshold_yearly(td,frthres)
  }
  
  result$annual = annual
  return(result)
}


get_frthreshold_yearly <- function(td,frthres){
  flname_all = setdiff(colnames(td),c("year","month","day"))
  td_fldata = td[,flname_all]
  doy = get_dayid(td$month,td$day)
  day365 = max(365, max(doy))
  years = unique(td$year)
  fl_doyfr = matrix(NA,day365,length(flname_all))
  out = matrix(NA,length(years),length(flname_all))
  colnames(out) = colnames(fl_doyfr) = flname_all
  
  for( fli in 1:length(flname_all) ){
    fl_doyfr[,fli] = get_doy_fr_0(td,flname_all[fli])
    idx_doy = which(fl_doyfr[,fli] > frthres)
    
    for( yi in 1:length(years)){
      id = which(td$year == years[yi])
      idx = id[which(doy[id] %in% idx_doy)]
      out[yi,fli] = mean(td_fldata[idx,fli],na.rm = T)
    }
  }
  out = cbind(years,out)
  return(out)
}