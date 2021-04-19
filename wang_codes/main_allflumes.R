source("func_allflumes.R")
### compute daily cor
result_dailycor = flumevegperc2data(out_dailycor)
### compute yearly cor
result_yearlycor = flumevegperc2data(out_yearlycor)
### get the best method
best_daily = get_best_method(result_dailycor$cor)
best_yearly = get_best_method(result_yearlycor$cor_yearly)
### get flumes' characteristics
out_fl = analyzebyflumes(flumes,calc_averagefr)
x_fl = flume2data(out_fl)

### plot
col_t1c0f1 = rgb(0,0,1,0.7)   
col_t0c0f0 = rgb(1,0.1,0,0.7)  
col_t1c1f1 = rgb(0,1,0,0.7)
colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)

par(mfrow = c(1,2))
tx = as.numeric(x_fl$fr)
ty = as.numeric(x_fl$max0)
bestwhat = rbind(best_daily,best_yearly)
for( pi in 1:2 ){
  for( ci in 1:length(colorname)){
    id = bestwhat[pi,] == ci
    plot(tx[id], ty[id], col = colorname[ci], xlim = c(0,1), ylim = c(0,80), pch = 19, las = 1)
    par(new = T)
  }
  par(new = F)
  text(x = tx, y = ty, labels = names(x$fr), cex = 0.5)
}


