loaddata <- function(datadir){
  data = list()
  ## load streamflow
  data$obs = read.csv(file.path(datadir, "obs.csv"))
  ## read vegetation
  data$ndvi = read.csv(file.path(datadir, "NDVI.csv"))
  data$evi = read.csv(file.path(datadir, "EVI.csv"))
  data$lai = read.csv(file.path(datadir, "LAI.csv"))
  ## perc x 8
  for (t in 0:1){
    for (c in 0:1){
      for (f in 0:1){
         filename = sprintf("perc_T%d_C%d_fr%d.csv", t,c,f)
         varname = sprintf("perc_t%dc%df%d", t, c, f)
         data[[varname]] = read.csv(file.path(datadir, filename))
      }
    }
  }
  return(data)
}


