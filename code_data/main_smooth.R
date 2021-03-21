library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)
datadir = "../../data"
source('func_process.R')
data = read.csv(file.path(datadir, 'runoff','runoff_raw.csv'))
data = preprocess(data)
write.csv(data,file.path(datadir, 'runoff', 'runoff.csv'))
