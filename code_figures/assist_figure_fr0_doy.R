get_doy_fr_0 <- function(td){
  ndata = setdiff(colnames(td),c("year","month","day"))
  doy = get_dayid(td$month,td$day)
  day365 = max(365, max(doy))
  fr_0 = matrix(NA,day365,length(ndata))
  for( fli in 1:length(ndata) ){
    for( dayi in 1:day365){
      id = which(doy == dayi & !is.na(td[,ndata[fli]]))
      fr_0[dayi,fli] = length(which(td[id,ndata[fli]] == 0)) / length(id)
    }
  }
  colnames(fr_0) = ndata
  return(fr_0)
}

plt_doy_fr_0 <- function(data,flname){
  td = data$obs
  fr_0 = get_doy_fr_0(td)
  
  plot(fr_0[,flname], type = "l", xlab ="", ylab = "", axes = F, col = "grey")
  axis(side = 1)
  #axis(side = 4, las = 1)
  box()
}

