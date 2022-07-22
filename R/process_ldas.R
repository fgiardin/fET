# extract data from LDAS data products

# PATH is the directory where all the NetCDF files from one product (e.g. NLDAS or GLDAS) are stored
# LAYER is the layer of the product you want to extract
# for GLDAS: Rainf_tavg, Evap_tavg, PotEvap_tavg, Rainf_f_tavg
# for NLDAS: PEVAP, APCP

# the function automatically tries to extract the variable (specified as 'layer')
# at the specified fluxnet2015 location (it returns NA if the location is outside of
# the NetCDF) and create a dataframe in the right format

# !!! for LDAS still need to transform to one value per day (it's hourly total data)
# GLDAS NOAH: 3 hours resolution

library(tidyverse)
library(raster)
library(ncdf4)
library(ingestr)

# list files
process_ldas <- function(path, layer, site_ID){

  # filter for site specified as site_ID
  siteinfo_fluxnet2015 <- ingestr::siteinfo_fluxnet2015 %>% # extract list of sites with info from ingestr
    dplyr::filter(sitename == site_ID)

  files <- list.files(path,"*.nc4", full.names = TRUE)

  output <- lapply(files, function(file){
    print(file) # print site_ID to keep track of it

    # extract date from file name of each GLDAS map (a bit tricky!)
    date_field <- do.call("rbind", str_split(file, "\\."))[,c(2,3)] # do.call divides
    # the filename in two pieces divided by points, and only takes piece number
    # 2 (where the date is, given the file name)
    date_field <- paste(date_field[1],date_field[2]) # merge two separate columns of matrix into one
    date_field <- gsub(" ","",date_field) # remove space created with previous step
    date_field <- gsub("A","",date_field) # replace the "A" before the data with a blank space

    dates <- lubridate::ymd_hm(date_field) # convert to date taking into account hours and minutes

    # extract variable specified in 'layer'
    s <- stack(file, varname = layer)

    df <- siteinfo_fluxnet2015 %>% # list of fluxnet sitenames with lon and lat
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

  })

  # bind together extracted output from each map
  output <- do.call("rbind", output)

  return(output)
}

