library(pracma)
calc_startday_annual <- function(td){
   result = list()
   annual= matrix(NA,1,365)
   for( lagi in 1:365 ){
      # print(sprintf('processing lag %d',lagi))
      ttd = get_year_lag(td,lagi)
      annual[lagi] = get_yearlycor(ttd$veg, ttd$perc, ttd$year)
   }
   result$startday_annual = annual
   return(result)
}
get_year_lag <- function(td,lag){
   td$doy = td$doy + lag
   id = which(td$doy > 365)
   td$doy[id] = td$doy[id] - 365
   td$year[id] = td$year[id] + 1
   return(td)
}






calc_drydays <- function(td){
   thres = linspace(0.05, 0.5, 10)  
   out = list(thres = thres)
   for (ti in 1:length(thres)){
      out$cor_dryday[ti] = get_yearlycor(td$veg, (td$perc < thres[ti])+0, td$year)
   }
   return(out)
}

calc_0s <- function(td){
   x = td$obs
   cd = get_cumulativedays(x == 0)
   id = which(diff(c(cd, 0)) < 0)
   out = list(len0 = cd[id], cd = cd[cd > 0])
   return(out)
}

get_cumulativedays<- function(x){
   y = matrix(NaN, length(x),1)
   idnan = which(!is.na(x))
   y[idnan] = 0
   y[idnan[x[idnan]>0]] = 1
   count = 0
   for (i in 1:length(y)){
      if (is.na(y[i]) || y[i] == 0){
         count = 0
      } else {
         count = count + 1
         y[i] = count
      }
   }
   y = y[,1]
   return(y)
}


calc_seasonlycor <- function(td){
   out = list()
   yr = td$year
   doy = td$doy
   doy = doy/max(doy)
   rg = linspace(0,1,5)
   for (ri in 1:(length(rg)-1)){
      id = which(doy >= rg[ri] & doy < rg[ri+1])
      out$cor_seasonal[ri] = get_yearlycor(td$veg[id], td$perc[id], yr[id])
   }
   return(out)
}


calc_seasonlycor_daily <- function(td){
   out = list()
   yr = td$year
   doy = td$doy
   doy = doy/max(doy)
   rg = linspace(0,1,5)
   for (ri in 1:length(rg)-1){
      id = doy >= rg[ri] & doy < rg[ri+1]
      out$cor_seasonal[ri] = mycor(td$veg[id], td$perc[id])
   }
   return(out)
}


calc_yearlycor_perm <- function(td){
   out = list()
   yr = td$year
   doy = td$doy
   out$cor_yearly = get_yearlycor(td$veg, td$perc, yr)
   nrep = 100
   cp = matrix(NA, nrep, 1)
   for( i in 1:nrep ){
      tyr = yr[randperm(length(yr))]
      cp[i] = get_yearlycor(td$veg,td$perc,tyr)
   }
   out$cor_perm = cp
   cp = matrix(NA, nrep, 1)
   for( i in 1:nrep ){
      tyr = scramble(yr, doy)
      cp[i] = get_yearlycor(td$veg,td$perc,tyr)
   }
   out$cor_perm_byday = cp
   return(out)
}

calc_yearlycor <- function(td){
  out = list()
  yr = td$year
  doy = td$doy
  out$cor_yearly = get_yearlycor(td$veg, td$perc, yr)
  return(out)
}

scramble <- function(yr, doy){
   doys = unique(doy)
   for (di in 1:length(doys)){
      id = which(doys[di] == doy)
      ty = yr[id]
      ty = ty[randperm(length(ty))]
      yr[id] = ty
   }
   return(yr)
}
get_yearlycor <- function(veg,perc,yr){
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


calc_01cor <- function(td){
   out = list()
   id = 1:length(td$perc)
   out$cor = mycor(td$veg[id], td$perc[id], method = "pearson", use = "pairwise")
   out$p_cor = mycor.test(td$veg[id], td$perc[id])
   id = which(td$obs == 0)
   out$cor_0 = mycor(td$veg[id], td$perc[id], method = "pearson", use = "pairwise")
   out$p_cor_0 = mycor.test(td$veg[id], td$perc[id])
   id = which(td$obs > 0)
   out$cor_non0 = mycor(td$veg[id], td$perc[id], method = "pearson", use = "pairwise")
   out$p_cor_non0 = mycor.test(td$veg[id], td$perc[id])
   
   return(out)
}


calc_daybydaycor <- function(td){
   doys = sort(unique(td$doy))
   # frs = matrix(NA, length(doys), 1)
   # for (di in 1:length(doys)){
   #    frs[di] = unique(td$fr[doys[di] == td$doy])
   # }
   ndoy = length(doys)
   out = list(doy = doys, cor = rep(NA, ndoy),
                    cor_0 = rep(NA, ndoy), cor_non0 = rep(NA, ndoy))
   for (di in 1:ndoy){
      id = which(td$doy == doys[di])
      out$cor[di] = mycor(td$veg[id], td$perc[id], method = "pearson", use = "pairwise")
      out$p_cor[di] = mycor.test(td$veg[id], td$perc[id])
      id = which(td$doy == doys[di] & td$obs == 0)
      out$cor_0[di] = mycor(td$veg[id], td$perc[id], method = "pearson", use = "pairwise")
      out$p_cor_0[di] = mycor.test(td$veg[id], td$perc[id])
      id = which(td$doy == doys[di] & td$obs > 0)
      out$cor_non0[di] = mycor(td$veg[id], td$perc[id], method = "pearson", use = "pairwise")
      out$p_cor_non0[di] = mycor.test(td$veg[id], td$perc[id])
   }
   return(out)
}

calc_fr <- function(td){
   fr = get_fr(td$doy, td$obs)
   return(fr)
}

calc_fr0s <- function(td){
  id = which(!is.na(td$ndvi))
  id = min(id) : max(id)
  fr = get_fr(td$doy[id], td$obs[id])
  return(fr)
}

get_fr <- function(doy, obs){
   doys = sort(unique(doy))
   frs = matrix(NA, length(doys), 1)
   for (fi in 1:length(frs)){
      id = which(doy == doys[fi] & !is.na(obs))
      if (length(id)> 0){
         frs[fi] = mean(obs[id] == 0)
      }
   }
   out = data.frame(doy = doys, fr = frs)
   return(out)
}

mycor <- function(x, y, ...){
   if (length(x) == 0){
      return(NA)
   }else{
      return(cor(x,y, ...))
   }
}

mycor.test <- function(x, y, ...){
   if (length(x) <= 2){
      return(NA)
   }else{
      return(cor.test(x,y)$p.value)
   }
}
