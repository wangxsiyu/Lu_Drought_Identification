# set up dir
library(rstudioapi)
cd = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)
# source files
source('func_percentiles.R')
d = read.csv('../../data/allflumes/obs.csv',header=T,stringsAsFactors=FALSE)
obs = preprocess(d)
# compute percentiles
tlm_rgday =  c(365)
cdpm_rgday = c(365)
fr_option =  c(0)
for (i in 1:length(tlm_rgday)){
  print(sprintf('loop %d/%d', i, length(tlm_rgday)))
  d_comb = for_allflumes(obs, comb, list(fr_option = fr_option[i]), list(tlm_rgday = tlm_rgday[i]), 
                         list(cdpm_rgday = cdpm_rgday[i], tlm_rgday = tlm_rgday[i]))
  write.csv(d_comb, file = sprintf('%s/perc_T%d_C%d_fr%d.csv',  savedir, -(tlm_rgday[i]/365-1), -(cdpm_rgday[i]/365-1), fr_option[i]))
}
