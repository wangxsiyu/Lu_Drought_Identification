
plt_heatmap <- function(data,flname){
  starty = 2003
  endy = 2017
  percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
  mename = toupper(substr(percname,6,11))
  par(mfrow = c(3,1))
  par(oma = c(1,1,1,1), mar = c(1,1,1,1))
  for( i in 1:length(percname) ){
    perc = data[[percname[i]]]
    get_heatmap_z(starty, endy, perc ,flname, list(method = mename[i], zlime = c(0,1)))
  }
}

get_heatmap_z <- function(starty, endy, perc, flname, param = list()){
  idxx = which(perc$year >= starty & perc$year <= endy)
  perc = perc[idxx,]
  perc$doy = get_dayid(perc$month,perc$day,option29 = 0)
  bf_heatmap(perc[,flname],perc$year,perc$doy,param)
}

bf_heatmap <- function(percc, year, doy, param = list()){
  library(fields)
  param00 = list(method = "", zlime = c(min(percc,na.rm = T),max(percc,na.rm = T)),color = rev(tim.colors(64)))
  param = modifyList(param00,param)
  tab = transmat(doy, year, percc)
  image.plot(x=tab$x, y = tab$y, z = tab$z, xlab = "",ylab = "", main = param$method, zlim = param$zlime,
             col = param$color)
}

transmat <- function(xs, ys, zs){
  idna = is.na(xs) | is.na(ys) | is.na(zs)
  xs = xs[!idna]
  ys = ys[!idna]
  zs = zs[!idna]
  x = unique(xs);
  y = unique(ys)
  z = matrix(NA, length(x), length(y))
  for (xi in 1:length(x)){
    for( yi in 1:length(y)){
      tid = (xs == x[xi] & ys == y[yi])
      if(is.na(sum(tid))){
        
      }else if(sum(tid) == 1){
        z[xi,yi] = zs[tid]
      }else{
        print(sprintf("xi=%d,yi=%d",xi,yi))
      }
    }
  }
  out = list(x=sort(x),y=sort(y),z=z)
  return(out)
}

get_dayid <- function(M,D, option29 = 0){
  y = matrix(NaN, length(D),1)
  ms = c(31,28 + option29,31,30,31,30,31,31,30,31,30)
  ms = c(0, cumsum(ms))
  for (i in 1:length(D)){
    y[i] = ms[M[i]] + D[i]
  }
  y = y[,1]
  return(y)
}
