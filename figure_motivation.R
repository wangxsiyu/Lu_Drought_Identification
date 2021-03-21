datadir = "/Users/Mengtian/Mengtian_Project/drought_identification/data"
funcdir = "/Users/Mengtian/Mengtian_Project/drought_identification/Lu_Drought_Identification"
setwd(funcdir)
source('loaddata.R')
data = loaddata(datadir);
# smooth
data$obs = preprocess(obs_d) 
# annual data
source('calc_annual.R')
data_year = calc_annual(data)
###################
source('assist_figure_motivation.R')
flname = "f001"
# get yearly zero fration
year_fr_0 = get_year_fr_0(data,flname)

# yearly time series
plt_year_ts(data_year$annual,flname, year_fr_0)

# correlation of runoff and vegetation
plt_runoff_veg(data, flname)

# zero flow and non-zero flow days' vegetation density
plt_veg_density(data, flname)
