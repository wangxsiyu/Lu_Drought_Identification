library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('func_process.R')
data = read.csv(file.path(datadir, 'runoff','runoff_raw.csv'))
tdata = preprocess(data)
write.csv(tdata,file.path(datadir, 'runoff', 'runoff.csv'))


############################################
# library(stringr)
# library(icesTAF)
# 
# for (t in c(0,1)){
#   for (c in c(0,1)){
#     for (fr in c(0,1)){
#       namepc = 'perc_T0_C0_fr0_1912_2020.csv'
#       perc = read.csv(file.path(datadir, 'perc',namepc))
#       for (i in 1:7){
#         tperc = preprocess_average(perc, i)
#         tstr = substr(namepc,1, str_length(namepc)-4)
#         tdir = file.path(datadir, 'perc', tstr)
#         mkdir(tdir)
#         write.csv(tperc,file.path(tdir, sprintf('%s_av%d.csv',tstr, i)))
#       }
#     }
#   }
# }
