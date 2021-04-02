loaddata <- function(datadir){
  data = list()
  ## load streamflow
  data$obs = read.csv(file.path(datadir, "runoff", "runoff.csv"))
  #data$obs = read.csv(file.path(datadir, "runoff", "runoff_raw.csv"))
  ## read vegetation
  data$ndvi = read.csv(file.path(datadir, "vegetation",  "ndvi.csv"))
  data$evi = read.csv(file.path(datadir, "vegetation", "evi.csv"))
  data$lai = read.csv(file.path(datadir, "vegetation", "lai.csv"))
  ## perc x 8
  for (t in 0:1){
    for (c in 0:1){
      for (f in 0:1){
         filename = sprintf("perc_T%d_C%d_fr%d_1912_2020.csv", t,c,f)
         varname = sprintf("perc_t%dc%df%d", t, c, f)
         data[[varname]] = read.csv(file.path(datadir, "perc", filename))
      }
    }
  }
  return(data)
}


loaddata_pqf <- function(datadir){
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
        varname = sprintf("perc_t%dc%df%d", t, c, f)
        data[[varname]] = read.csv(file.path(datadir, "perc", filename))
        
        quant_filename = sprintf("quant_T%d_C%d_fr%d_1912_2020.csv", t,c,f)
        quantname = sprintf("quant_t%dc%df%d", t, c, f)
        data[[quantname]] = read.csv(file.path(datadir, "perc", quant_filename))
        
        fr_filename = sprintf("fr_T%d_C%d_fr%d_1912_2020.csv", t,c,f)
        frname = sprintf("fr_t%dc%df%d", t, c, f)
        data[[frname]] = read.csv(file.path(datadir, "perc", fr_filename))
      }
    }
  }
  return(data)
}

loaddata_h8v5 <- function(datadir){
  data = list()
  ## load streamflow
  data$obs = read.csv(file.path(datadir, "runoff", "runoff.csv"))
  #data$obs = read.csv(file.path(datadir, "runoff", "runoff_raw.csv"))
  ## read vegetation
  data$ndvi = read.csv(file.path(datadir, "veg_h8v5",  "ndvi.csv"))
  data$evi = read.csv(file.path(datadir, "veg_h8v5", "evi.csv"))
  data$lai = read.csv(file.path(datadir, "veg_h8v5", "lai.csv"))
  ## perc x 8
  for (t in 0:1){
    for (c in 0:1){
      for (f in 0:1){
        filename = sprintf("perc_T%d_C%d_fr%d_1912_2020.csv", t,c,f)
        varname = sprintf("perc_t%dc%df%d", t, c, f)
        data[[varname]] = read.csv(file.path(datadir, "perc", filename))
      }
    }
  }
  return(data)
}
