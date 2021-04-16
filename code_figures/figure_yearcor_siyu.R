library(pracma)
flname = "f001"
vegname = c("ndvi","evi","lai")
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
r0 = matrix(NA,length(vegname),length(percname))
re = matrix(list(),length(vegname),length(percname))
rownames(r0) = rownames(re) = vegname
colnames(r0) = colnames(re) = percname
par(mfrow = c(3,3))
par(oma = c(1,2,1,1), mar = c(1,2,1,1))
for( i in 1:length(vegname)){
  id = which(!is.na(data[[vegname[i]]][,flname]))
  yr = data$obs$year[id]
  veg = data[[vegname[i]]][id,flname]
  for( j in 1:length(percname)){
    perc = data[[percname[j]]][id,flname]
    r0[i,j] = get_yearcor_siyu(veg,perc,yr)
    rs = NULL
    for( ii in 1:100 ){
      tyr = yr[randperm(length(yr))]
      rs[ii] = get_yearcor_siyu(veg,perc,tyr)
    }
    re[[i,j]] = rs
    plot(density(re[[i,j]]),xlab = "", ylab = "", main = paste(vegname[i],percname[j],sep = ""))
    abline(v = r0[i,j])
  }  
}



get_yearcor_siyu <- function(veg,perc,yr){
  years = unique(yr)
  v = p = matrix(NA,length(years),1)
  for( yi in 1:length(years)){
    id = which(yr == years[yi])
    v[yi,] = mean(veg[id],na.rm = T)
    p[yi,] = mean(perc[id],na.rm = T)
  }
  out = cor(v,p,use = "pairwise")
  return(out)
}





