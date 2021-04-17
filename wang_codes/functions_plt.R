plot_byflume <- function(result, pltfunc, param = list()){
  col_t1c0f1 = rgb(0,0,1,0.7)   
  col_t0c0f0 = rgb(1,0.1,0,0.7)  
  col_t1c1f1 = rgb(0,1,0,0.7)
  colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)
  param0 = list(colorname = colorname)
  param = modifyList(param0, param)
  par(mfrow = c(4,6))
  par(oma = c(2,2,1,1), mar = c(2,2,1,1))
  flname_all = names(result)
  for( fli in 1:length(flname_all) ){
      pltfunc(result[[flname_all[fli]]], param)
  }
}



plt_dryday <- function(result, param = list()){
  param0 = list(vegname = "ndvi", ylm = c(-1, 1.1), alpha = 0.05, xlm = c(0,0.55))
  param = modifyList(param0, param)
  alpha = param$alpha
  xlm = param$xlm
  ylm = param$ylm
  vars = paste("vegperc", param$vegname, "perc", c("t1c0f1","t0c0f0","t1c1f1"), sep = "_")
  nvar = length(vars)
  for (vi in 1:nvar){
    tthres = result[[vars[vi]]]$thres
    tcor = result[[vars[vi]]]$cor_dryday
    plot(tthres, tcor, col = param$colorname[vi],  type = 'l', xlim = xlm, ylim = ylm)
    par(new = T)
    # issig = (result[[vars[vi]]][[paste('p_',param$select, sep = "")]][id] < alpha) + 0
    # issig[!issig] = NA
    # plot(doy, issig * (vi/nvar*0.1 + 1), lwd = 2, col = param$colorname[vi],  type = 'l', xlim = xlm, ylim = ylm)
    # par(new = T)
  }
  par(new = F)
}


plt_daybydaycor <- function(result, param = list()){
  param0 = list(issort = F, vegname = "ndvi", select = "cor", ylm = c(0, 1.1), alpha = 0.05)
  param = modifyList(param0, param)
  ylm = param$ylm
  alpha = param$alpha
  doy = sort(unique(result$vegperc_ndvi_perc_t0c0f0$doy))
  fr = result$fr
  if (param$issort){
    id = order(fr[doy])
    xlm = c(0, length(doy))
    plot(1:length(id), sort(fr[doy]), type = "l", col = "grey", xlim = xlm, ylim = c(0,1), las = 1)
    doy = 1:length(doy)
  } else {
    id = 1:length(doy)
    xlm = c(0, 365)
    plot(1:365, fr, type = "l", col = "grey", xlim = xlm, ylim = c(0,1), las = 1)
  }
  par(new = T)
  vars = paste("vegperc", param$vegname, "perc", c("t1c0f1","t0c0f0","t1c1f1"), sep = "_")
  nvar = length(vars)
  for (vi in 1:nvar){
    tcor = result[[vars[vi]]][[param$select]]
    plot(doy, tcor[id], col = param$colorname[vi],  type = 'l', xlim = xlm, ylim = ylm)
    par(new = T)
    issig = (result[[vars[vi]]][[paste('p_',param$select, sep = "")]][id] < alpha) + 0
    issig[!issig] = NA
    plot(doy, issig * (vi/nvar*0.1 + 1), lwd = 2, col = param$colorname[vi],  type = 'l', xlim = xlm, ylim = ylm)
    par(new = T)
  }
  par(new = F)
}

plt_perm <- function(result, param = NULL){
  xlm = c(-0.5, 1)
  vegname = "ndvi"
  vars = paste("vegperc", vegname, "perc", c("t1c0f1","t0c0f0","t1c1f1"), sep = "_")
  nvar = length(vars)
  td1 = td2 = list()
  ymax = matrix(NA, nvar, 1)
  for (vi in 1:nvar){
    td1[[vi]] = density(result[[vars[vi]]]$cor_perm)
    td2[[vi]] = density(result[[vars[vi]]]$cor_perm_byday)
    ymax[vi] = max(c(td1[[vi]]$y, td2[[vi]]$y))
  }
  for (vi in 1:nvar){
    plot(td1[[vi]]$x, td1[[vi]]$y/ymax[vi]/nvar + (vi-1)/nvar, col = "gray", type = 'l', xlim = xlm, ylim = c(0,1))
    par(new = T)
    plot(td2[[vi]]$x, td2[[vi]]$y/ymax[vi]/nvar + (vi-1)/nvar, col = "black", type = 'l',xlim = xlm, ylim = c(0,1))
    par(new = T)
    tc = result[[vars[vi]]]$cor_yearly
    plot(x = c(tc, tc), y = c((vi-1)/nvar, vi/nvar), col = param$colorname[vi], type = 'l',xlim = xlm, ylim = c(0,1))
    par(new = T)
  }
  par(new = F)
}

plt_runoff <- function(result, params){
  doy = get_dayid(result$month, result$day)
  tab = transmat(doy, result$year, result$obs)
  image(x=tab$x, y = tab$y, z = tab$z, xlab = "",ylab = "")
}


transmat <- function(xs, ys, zs){
  idna = is.na(xs) | is.na(ys) | is.na(zs)
  xs = xs[!idna]
  ys = ys[!idna]
  zs = zs[!idna]
  x = sort(unique(xs))
  y = sort(unique(ys))
  z = matrix(NA, length(x), length(y))
  for (xi in 1:length(x)){
    for( yi in 1:length(y)){
      tid = (xs == x[xi] & ys == y[yi])
      if (sum(tid) == 1){
        z[xi,yi] = zs[tid]
      }else if (sum(tid) >1){
        z[xi,yi] = mean(zs[tid])
      }
    }
  }
  out = list(x=x,y=y,z=z)
  return(out)
}

plt_dailycor <- function(result, params){
  vegname = "ndvi"
  vars = paste("vegperc", vegname, "perc", c("t1c0f1","t0c0f0","t1c1f1"), sep = "_")
  nvar = length(vars)
  td1 = td2 = list()
  ymax = matrix(NA, nvar, 1)
  for (vi in 1:nvar){
    tr = result[[vars[vi]]]
    plot(c(tr$cor, tr$cor_0, tr$cor_non0), type = 'l', xlim = c(0.5, 3.5), ylim = c(-1,1), col = params$colorname[vi])
    par(new = T)
  }
  par(new = F)
}


plt_seasonal <- function(result, params){
  vegname = "ndvi"
  vars = paste("vegperc", vegname, "perc", c("t1c0f1","t0c0f0","t1c1f1"), sep = "_")
  nvar = length(vars)
  td1 = td2 = list()
  ymax = matrix(NA, nvar, 1)
  for (vi in 1:nvar){
    tr = result[[vars[vi]]]
    plot(tr$cor_seasonal, type = 'l', xlim = c(0.5, 4.5), ylim = c(-1,1), col = params$colorname[vi])
    par(new = T)
  }
  par(new = F)
}


plt_dist0 <- function(result, param = NULL){
  bks = c(linspace(0, 500,50), 10000)
  nbin = length(bks) - 2
  ht1 = hist(result$len0, breaks = bks, plot = F)
  plot(ht1$mids[1:nbin], ht1$counts[1:nbin]/max(ht1$counts[1:nbin]), col = "black", type = 'l')
  ht2 = hist(result$cd, breaks = bks, plot = F)
  lines(ht2$mids[1:nbin], ht2$counts[1:nbin]/max(ht2$counts[1:nbin]), col = "gray")
}

plt_yrst <- function(result, params = NULL){
  vegname = "ndvi"
  vars = paste("vegperc", vegname, "perc", c("t1c0f1","t0c0f0","t1c1f1"), sep = "_")
  nvar = length(vars)
  for (vi in 1:nvar){
    tr = result[[vars[vi]]]$startday_annual
    tr = as.numeric(tr)
    plot(tr, type = 'l', xlim = c(0.5, 365.5), ylim = c(-1,1), col = params$colorname[vi])
    par(new = T)
  }
  par(new = F)
}
