library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
#source('loaddata.R')
select_yr = c(1956,1980,2000) #c(1966,1976,1986,1996,2006)
data = loaddata_yrselect(datadir,select_yr)
data_year = calc_annual(data)$annual
###################
percname = c("perc_t1c0f1","perc_t0c0f0","perc_t1c1f1")
vegname = c("ndvi","evi","lai")
veg_perc_cor_yr = matrix(list(),length(vegname),length(percname))
for (i in 1:length(vegname)){
  for (j in 1:length(percname)){
    yr_cor = NULL
    for( yr in 1:length(select_yr)){
      percname_yr = paste(percname[j],"_",select_yr[yr], sep = "")
      perc1956 = paste(percname[j],"_1956", sep = "")
      yr_start = data[[percname_yr]][,"year"][1]
      #idx = which(data[[vegname[i]]][,"year"] >= yr_start & data[[vegname[i]]][,"year"] <= 2020)
      #tveg = data[[vegname[i]]][idx,flname]
      idx = which(data[[perc1956]][,"year"] >= yr_start & data[[perc1956]][,"year"] <= 2020)
      tveg = data[[perc1956]][idx,flname]
      tperc = data[[percname_yr]][,flname]
      yr_cor[yr] = cor(tveg,tperc,use = "pairwise")
    }
    names(yr_cor) = select_yr
    veg_perc_cor_yr[[i,j]] = yr_cor
  }
}
colnames(veg_perc_cor_yr) = percname
rownames(veg_perc_cor_yr) = vegname

###### plot
par(mfrow = c(1,3))
par(oma = c(2,2,1,1), mar = c(1.5,2,1,1))
col_t1c0f1 = rgb(0,0,1,0.7)   
col_t0c0f0 = rgb(1,0.1,0,0.7)  
col_t1c1f1 = rgb(0,1,0,0.7)
colorname = c(col_t1c0f1,col_t0c0f0,col_t1c1f1)

for (i in 1:length(vegname)){
  for (j in 1:length(percname)){
    plot(veg_perc_cor_yr[[i,j]], type = "o", col = colorname[j],axes = F, pch = 19,
         xlab = "", ylab = "", main = "", ylim = c(0,1))
    par(new = T)
  }
  axis(side = 1, at = 1:length(select_yr), labels = select_yr)
  axis(side = 2, las = 1)
  mtext(side = 3, line = 0.5, text = toupper(vegname[i]))
  box()
  par(new = F)
}
legend("topleft", legend = percname, lty = 1, pch = 19, col = colorname, 
       y.intersp = 0.8, x.intersp = 0.3, bty = "n")


loaddata_yrselect <- function(datadir,select_yr){
  data = list()
  ## load streamflow
  data$obs = read.csv(file.path(datadir, "runoff", "runoff.csv"))
  ## read vegetation
  data$ndvi = read.csv(file.path(datadir, "veg_h8v5",  "ndvi.csv"))
  data$evi = read.csv(file.path(datadir, "veg_h8v5", "evi.csv"))
  data$lai = read.csv(file.path(datadir, "veg_h8v5", "lai.csv"))
  ## perc x 8
  for (t in 0:1){
    for (c in 0:1){
      for (f in 0:1){
        for (yr in select_yr){
          filename = sprintf("perc_T%d_C%d_fr%d_%d.csv", t,c,f,yr)
          varname = sprintf("perc_t%dc%df%d_%d", t, c, f, yr)
          data[[varname]] = read.csv(file.path("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/siyu_new/results/smooth", filename))
        }
      }
    }
  }
  return(data)
}

