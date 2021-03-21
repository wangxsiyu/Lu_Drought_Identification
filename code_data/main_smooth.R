library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('func_process.R')
data = read.csv(file.path(datadir, 'runoff','runoff_raw.csv'))
tdata = preprocess(data)
write.csv(tdata,file.path(datadir, 'runoff', 'runoff.csv'))


############################################
library(stringr)
namepc = 'perc_T0_C0_fr0_1912_2020.csv'
perc = read.csv(file.path(datadir, 'perc',namepc))
for (i in 1:7){
  tperc = preprocess_average(perc, i)
  tstr = substr(namepc,1, str_length(namepc)-4)
  write.csv(tperc,file.path(datadir, 'perc', tstr, sprintf('%s_av%d.csv',tstr, i)))
}
