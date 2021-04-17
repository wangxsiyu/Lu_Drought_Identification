# set up working dir
library(rstudioapi)
funcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(funcdir)

# load data
datadir = "../../data/allflumes"
source("loaddata.R")
data = loaddata(datadir);

# analyze flume by flume
source("function_structures.R")
source("functions.R")
source("functions_plt.R")
flumes = data2flume(data)

# calculate fraction
out_fr = analyzebyflumes(flumes, calc_fr)
fr = flume2data(out_fr, c("doy"))

# daily corr
out_dailycor = analyzebyflumes_vegperc(flumes, calc_01cor)
plot_byflume(out_dailycor, plt_dailycor)

# day by day corr
out_daybydaycor = analyzebyflumes_vegperc(flumes, calc_daybydaycor)
result_daybydaycor = flumevegperc2data(out_daybydaycor, c("doy"))
out_daybydaycor = combine(out_daybydaycor, out_fr)
corv = "cor"
ylm = c(-1, 1.1)
alpha = 0.1
plot_byflume(out_daybydaycor, plt_daybydaycor, list(select = corv, ylm = ylm, alpha = alpha))
plot_byflume(out_daybydaycor, plt_daybydaycor, list(issort = T, select = corv, ylm = ylm,  alpha = alpha))

# permutation test
out_perm = analyzebyflumes_vegperc(flumes, calc_yearlycor)
result_permcor = flumevegperc2data(out_perm)
plot_byflume(out_perm, plt_perm)

# runoff (check baseflow)
plot_byflume(flumes, plt_runoff)

# seasonal
out_seasonal = analyzebyflumes_vegperc(flumes, calc_seasonlycor)
plot_byflume(out_seasonal, plt_seasonal)

# distribution of 0s
out_0s = analyzebyflumes(flumes, calc_0s)
plot_byflume(out_0s, plt_dist0)

# yearly vs startdate
out_yrst = analyzebyflumes_vegperc(flumes, calc_startday_annual)
plot_byflume(out_yrst, plt_yrst)

# dry days (threshold) vs yearly
out_drydays = analyzebyflumes_vegperc(flumes, calc_drydays)
plot_byflume(out_drydays, plt_dryday)

