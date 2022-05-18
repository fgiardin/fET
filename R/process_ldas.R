# process data

library(tidyverse)
library(raster)
library(ncdf4)

# list files
process_ldas <- function(path, layer){

  # needs to be replaced with a function call
  # from ingestr
  load("siteinfo_fluxnet2015.RData")

  files <- list.files(path,"*.nc4", full.names = TRUE)

  date_field <- do.call("rbind", str_split(files, "\\."))[,2]
  date_field <- gsub("A","",date_field)

  dates <- as.Date(
    date_field,
    "%Y%m%d"
  )

  # you will need to specify which layer of the NLDAS product
  # you want
  if(missing(layer)){
    s <- stack(files)
  } else {
    s <- stack(files, varname = layer)
  }

  df <- siteinfo_fluxnet2015 %>%
    rowwise() %>%
    do({

      values <- raster::extract(s, matrix(c(.$lon, .$lat),1,2))[1,]

      data.frame(
        sitename = .$sitename,
        lon = .$lon,
        lat = .$lat,
        date = dates,
        values = values
      )
    })

  return(df)
}


