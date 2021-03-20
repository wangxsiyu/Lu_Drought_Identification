datadir = "/Users/wang/Downloads/Archive/SIYUTEMPLATE/data"
source('loaddata.R')
data = loaddata(datadir);
###################
source('assist_figure_daily.R')
plt_flume_daily(data, "f001")
plt_flume_daily(data, "f002")
