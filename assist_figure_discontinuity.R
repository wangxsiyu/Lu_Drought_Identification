plt_discontinuty <- function(idx,data,flname){
  obs = data$obs[,flname]
  col_t1c0f1 = rgb(0,0,1,0.7)    
  col_t0c0f0 = rgb(1,0.1,0,0.7) 
  col_t1c1f1 = rgb(0,1,0,0.7)
  
  par(mfrow = c(1,1))
  par(oma = c(2,2,1,1), mar = c(1,1,1,1))
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  for (i in 1:length(percname)){
    perc = data[[percname[i]]][,flname]
    plt_tpp(idx,obs,perc,list(max_du = 200, af_du = 200, ylime = c(0,1), color = colorname[i]))
    par(new = T)
  }
  legend("bottomleft",legend = percname, col = colorname,
         lty = 1, bty = "n", y.intersp = 0.8)
  par(new = F)
}



plt_tpp <- function(idxx,obs,perccc,param = list()){
  param00 = list(max_du = 100, af_du = 100, ylime = c(0,1), color = "black")
  param = modifyList(param00,param)
  cdpm_siyu = get_cumulativedays(obs==0)
  cdpm_siyu_re = calc_0value(obs==0,"reverse")
  cdpm_tot = cdpm_siyu + cdpm_siyu_re
  idx_siyu = cdpm_tot > param$max_du
  idx_l = which(idx_siyu & cdpm_siyu == 1 & idxx)
  idx_bf = idx_l# - 10
  idx_af = idx_l + param$af_du
  tpp = matrix(NA,length(idx_l),param$af_du+1) #+11)
  for( i in 1:length(idx_l)){
    tpp[i,] = perccc[idx_bf[i]:idx_af[i]]
    #plot(x = -10:param$af_du, y = tpp[i,],type = "l", ylim = param$ylime, col = "grey")
    #par(new = T)
  }
  tpp_cm = colMeans(tpp,na.rm = T)
  plot(x = 0:param$af_du, y = tpp_cm,type = "l", xlim = c(0,param$af_du), ylim = param$ylime, col = param$color,las = 1,
       xlab = "", ylab = "", main = "")
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

calc_0value <- function(Q0, param_cdpm = 'linear'){
  library(pracma)
  cd = get_cumulativedays(Q0)
  if (param_cdpm == 'triangular'){
    cd_rev = get_cumulativedays(Q0[length(Q0):1])
    cd_rev = cd_rev[length(cd_rev):1]
    # cd = abs((cd - cd_rev)/2) 
    te = cbind(cd, cd_rev)
    id = max.col(-te, ties.method ="first")
    cd = arrayfun(function(x){te[x, id[x]]}, 1:length(id))
  }else if(param_cdpm == 'reverse'){
    cd_rev = get_cumulativedays(Q0[length(Q0):1])
    cd_rev = cd_rev[length(cd_rev):1]
    cd = cd_rev
  }
  cd[cd == 0] = NaN
  return(cd)
}

