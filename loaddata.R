loaddata <- function(datadir){
  data = list()
  # load streamflow
  data$obs = read.csv(file.path(datadir, "runoff", "runoff.csv"))
  # read vegetation
  data$ndvi = read.csv(file.path(datadir, "vegetation",  "ndvi.csv"))
  data$evi = read.csv(file.path(datadir, "vegetation", "evi.csv"))
  data$lai = read.csv(file.path(datadir, "vegetation", "lai.csv"))
  ## perc x 8
  for (t in 0:1){
    for (c in 0:1){
      for (f in 0:1){
         filename = sprintf("perc_T%d_C%d_fr%d_1912_2020.csv", t,c,f)
         varname = sprintf("perc_t%dc%df%d", 1 - t, 1 - c,f)
         data[[varname]] = read.csv(file.path(datadir, "perc", filename))
      }
    }
  }
  return(data)
}
