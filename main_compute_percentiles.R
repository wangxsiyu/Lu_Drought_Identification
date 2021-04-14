library(rstudioapi)
cd = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
# source files
source('./code_format.R')
source('./drought_identification_siyunew.R')
# read runoff data
d = read.csv('../data/USGSall_WGEW_flume_observations.csv',header=T,stringsAsFactors=FALSE)
#d = read.csv("/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/x9471000/otherflumes_obs_mm.csv") #read.csv("/Users/Mengtian/Mengtian_Project/drought_identification/data/runoff/runoff_raw.csv") #"/Users/Mengtian/MENGTIAN/Lu_DroughtIdentification/data/pieter_newdata/obs_nosmooth.csv")
d = format_csv(d)
#yrselect = 1956 #1966 1976 1986 1996 2006
smooth = "smooth" #"no_smooth"
if ( smooth == "smooth"){
  dd = preprocess(d)
  obs = dd
  #idx_select = which(obs$year == yrselect)
  #obs = obs[idx_select:nrow(obs),]
}else if( smooth == "no_smooth"){
  obs = d
}

##### 20210414 mengtian
datacol = setdiff(colnames(obs),c("year","month","day","X"))
for( i in 1:length(datacol) ){
  td = obs[,datacol[i]]
  idx = which(td>0)[1]
  obs[1:(idx-1),datacol[i]] = NA
}
###########################
savedir = paste('./results/',smooth,sep = "")
# compute percentiles
tlm_rgday =  c(0,  0,  365, 365, 0,  0,  365, 365)#, 10, 30)
cdpm_rgday = c(0, 365, 365,  0,  0, 365, 365,  0 )#, 10, 30)
fr_option =  c(0,  0,   0,   0,  1,  1,   1,   1 )
# print('tlm')
# for (i in 1:length(tlm_rgday)){
#   d_tlm = for_allflumes(d, tlm, list(tlm_rgday = tlm_rgday[i]))
#   write.csv(d_tlm, file = sprintf('%s/perc_tlm%d.csv',  savedir, tlm_rgday[i]))
# }
# print('cdpm')
# for (j in 1:length(cdpm_rgday)){
#   d_cdpm = for_allflumes(d, cdpm, list(cdpm_rgday = cdpm_rgday[j]))
#   write.csv(d_tlm, file = sprintf('%s/perc_cdpm%d.csv',  savedir, cdpm_rgday[j]))
# }
print('comb')
isquant = 0
for (i in 1:length(tlm_rgday)){
  print(sprintf('loop %d', i))
  d_comb = for_allflumes(isquant, obs, comb, list(tlm_rgday = tlm_rgday[i]), 
                         list(cdpm_rgday = cdpm_rgday[i], tlm_rgday = tlm_rgday[i], fr_option = fr_option[i]))
  write.csv(d_comb, file = sprintf('%s/perc_T%d_C%d_fr%d.csv',  savedir, -(tlm_rgday[i]/365-1), -(cdpm_rgday[i]/365-1), fr_option[i]))
}
d_fr = for_allflumes(dd,get_fraction)
