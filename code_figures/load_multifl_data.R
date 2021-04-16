
# obs_d = read.csv("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/x9471000/otherflumes_obs_mm.csv")
# for( i in 1:length(veg) ){
#   datadir = paste("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/MODIS NDVI",veg[i],sep = "/")
#   veg_data = read.csv( file = paste(datadir,"/",veg[i],".csv",sep = ""))
#   veg_ext = modis_preprocess(obs_d,veg_data)
#   write.csv(veg_ext,file = paste(datadir,"/",veg[i],'ext.csv',sep = ""))
#   print(sprintf('veg %d', i))
# }
# 
# ## obs smooth
# source('/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification/code_data/func_process.R')
# obs_d = read.csv("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/x9471000/otherflumes_obs_mm.csv")
# tdata = preprocess(obs_d)
# write.csv(tdata,file = ("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/x9471000/otherflumes_obs_afsmooth.csv"))

data = list()
## load streamflow
data$obs = read.csv("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/x9471000/otherflumes_obs_afsmooth.csv")
## read vegetation
datadir = "/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/MODIS NDVI/"
data$ndvi = read.csv(paste(datadir, "NDVI/NDVIext.csv",sep = ""))
data$evi = read.csv(paste(datadir, "EVI/EVIext.csv",sep = ""))
data$lai = read.csv(paste(datadir, "LAI/LAIext.csv",sep = ""))
## perc x 8
for (t in 0:1){
  for (c in 0:1){
    for (f in 0:1){
      filename = sprintf("perc_T%d_C%d_fr%d.csv", t,c,f)
      varname = sprintf("perc_t%dc%df%d", t, c, f)
      data[[varname]] = read.csv(paste("/Users/Mengtian/Mengtian_Project/drought_identification/data/otherfl_perc/", filename, sep = ""))
    }
  }
}






loaddata_rm1yr <- function(datadir){
  data = list()
  ## load streamflow
  data$obs = read.csv(file.path(datadir, "runoff", "runoff.csv"))
  #data$obs = read.csv(file.path(datadir, "runoff", "runoff_raw.csv"))
  ## read vegetation
  data$ndvi = read.csv(file.path(datadir, "vegetation",  "ndvi.csv"))
  data$evi = read.csv(file.path(datadir, "vegetation", "evi.csv"))
  data$lai = read.csv(file.path(datadir, "vegetation", "lai.csv"))
  ## perc x 8
  for (t in 0:1){
    for (c in 0:1){
      for (f in 0:1){
        filename = sprintf("perc_T%d_C%d_fr%d.csv", t,c,f)
        varname = sprintf("perc_t%dc%df%d", t, c, f)
        data[[varname]] = read.csv(paste("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/siyu_new/results/smooth/", filename, sep = ""))
      }
    }
  }
  return(data)
}



