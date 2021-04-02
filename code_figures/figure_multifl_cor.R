library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('loaddata.R')
data = loaddata(datadir);
source('calc_annual.R')
data_an = calc_annual(data)$annual
source("calc_residue.R")
data_residue = calc_residue(data)
############ pieter data #####################
source('assist_figure_multifl_cor.R')
source('assist_figure_residue.R')
flname = c("f001","f002","x9472050") #nobaseflow
## daily
plt_multi_fl(get_perc_veg_cor, data, flname, isres = 0)
## yearly
plt_multi_fl(get_perc_veg_cor, data_an, flname, isres = 0)
## residue
plt_multi_fl(plt_residue_box, data_residue, flname, isres = 1)


############ new h5v8 data #####################
source('loaddata.R')
data = loaddata_h8v5(datadir)
data_an = calc_annual(data)$annual
data_residue = calc_residue(data)
flname = setdiff(colnames(data$obs),c("year","month","day","X"))
area = read.xlsx(paste(datadir,"sanpedro_area.xlsx",sep = "/"))
flname_sort = area[order(area$area,decreasing = T),"flume"]

flname_select = c("x9472050","f001","f002","f006","f015","f009","x9470700","f010","f007",    #nb
                  "x9471380","x9470800","x9470750","x9471310",    #bu
                  "x9471550","x9471000","x9470500","x9471400")#,    #bd
                  #"f003","f011","f125","f121","f104","f103","f112","f102","f106","f105")    
## daily
plt_multi_fl(get_perc_veg_cor, data, flname_select)
## yearly
plt_multi_fl(get_perc_veg_cor, data_an, flname_select, isperc = 1)
## residue
plt_multi_fl(plt_residue_box, data_residue, flname_select, isres = 1)


################## multi flumes' area / fraction of 0 / element ###############################
area = read.xlsx(paste(datadir,"sanpedro_area.xlsx",sep = "/"))
idx = NULL
for( i in 1:length(flname_select)){idx[i] = which(area$flume == flname_select[i])}
fl_name = flname_select

{
par(mfrow = c(5,1))
par(oma = c(1.5,2.5,1,1), mar = c(1.5,2,1.5,1))

###### area
plt_multi_fl_fac(area$area[idx],fl_name)
mtext(side = 2, line = 3, text = "Area(km^2)")

###### fraction of zero 
fr0_yr = NULL
for( fli in 1:length(fl_name) ){
  fr0_yr = get_year_fr_0(data,fl_name[fli])
  #boxplot(fr0_yr, at = fli, xlim = c(0,length(area$flume)+1), ylim = c(0,1), las = 1)
  plot(x = fli, y = mean(fr0_yr,na.rm = T),
       xlim = c(0,length(fl_name)+1), ylim = c(0,1), las = 1, axes = F, pch = 19)
  par(new = T)
}
axis(side = 1, at = 1:length(fl_name), labels = F)
axis(side = 2, las = 1)
box()
text(x = 1:length(fl_name),y = par("usr")[3]-0.03,labels = fl_name,xpd = NA,srt = 30,adj = 1,cex = 1)
mtext(side = 2, line = 2.5, text = "Fraction of zero flows")

####### elevation
plt_multi_fl_fac(area$elevation[idx],fl_name)
mtext(side = 2, line = 2.5, text = "Elevation(m)")

####### data length
obs = data$obs
year = unique(obs$year)
fr_nonan = NULL
for( fli in 1:length(fl_name)){
#  for( i in 1:length(year) ){
#    idx_y = which(obs$year == year[i])
    re = obs[,fl_name[fli]]
    fr_nonan[fli] = length(which(!is.na(re)))/365
#  }
  
}
names(fr_nonan) = fl_name
plt_multi_fl_fac(fr_nonan,fl_name)
mtext(side = 2, line = 2.5, text = "Data length")


##################
vegname = c("ndvi","evi","lai")
fl_non = NULL 
for( fli in 1:length(fl_name)){
  sum_nontobs = NULL
  for (i in 1:length(vegname)){
    tobs = data[["obs"]][,fl_name[fli]]
    tveg = data[[vegname[i]]][,fl_name[fli]]
    a = !is.na(tobs) & !is.na(tveg)
    sum_nontobs[i] = sum(a)
  }
  names(sum_nontobs) = vegname
  fl_non = rbind(fl_non,sum_nontobs)
}
rownames(fl_non) = fl_name
color = c("pink","yellow","purple")
lty = c(1,2,1)
ymin = min(fl_non)
ymax = max(fl_non)
for( i in 1:length(vegname)){
  plot(fl_non[,i], type = "o", col = color[i], ylim = c(ymin,ymax), xlim = c(0.5,length(fl_name)+0.5), pch = 19,
       xlab = "", ylab = "", main = "", axes = F, lty = lty[i])
  par(new = T)
}
axis(side = 1, at = 1:length(fl_name), labels = F)
text(x = 1:length(fl_name),y = par("usr")[3]-0.03,labels = fl_name,xpd = NA,srt = 30,adj = 1,cex = 1)
axis(side = 2, las = 1)
box()
mtext(side = 2, line = 2.5, text = "Data length used for cor")
}

legend("topleft", legend = c("NDVI","EVI","LAI"), col = color, lty = lty, pch = 19)
