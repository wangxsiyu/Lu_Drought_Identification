calc_averagefr <- function(td){
  fr = get_fr(td$doy, td$obs)
  out = list()
  out$fr = mean(fr$fr == 1)
  te = get_cumulativedays(fr$fr == 1)
  out$max0 = max(te, na.rm = T) 
  return(out)
}


get_best_method <- function(result){
  percnames = c("t1c0f1","t0c0f0","t1c1f1")
  vegname = "ndvi"
  re = NULL
  for( pi in 1:length(percnames)){
    tnm = sprintf("vegperc_%s_perc_%s", vegname, percnames[pi])
    re = rbind(re,result[[tnm]])
  }
  library(qlcMatrix)
  xx = as.numeric(colMax(re))
  yy = as.matrix(re)
  zz = yy - matrix(1,3,1) %*% t(as.matrix(xx))
  zz = (zz==0)+0
  maxdaily = t(as.matrix(1:3)) %*% zz
  return(maxdaily)
}

