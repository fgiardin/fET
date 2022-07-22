# Extract and read global topographic index (GTI)

library(tidyverse)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis

# Note: Given its size (~10 GB), the NetCDF dataframe was not uploaded to the repo
# You can download it here: https://catalogue.ceh.ac.uk/documents/6b0c4358-2bf3-4924-aa8f-793d468b92be

# load data and see structure
nc_data <- nc_open('~/data/gti_marthews/ga2.nc')
# Save the print(nc) dump to a text file
{
  sink('ga2_metadata.txt')
  print(nc_data)
  sink()
}

# load table with sites to extract site codes and lon/lat info
load("manuscript/Figures/dataframes/table1_raw.RData")
df_sites <- table1 %>%
  dplyr::select(name_site, lon, lat, cluster)

# load NetCDF dataframe with raster package
rasta <- raster::raster("~/data/gti_marthews/ga2.nc")

# extract gti values at fluxnet sites locations
df_gti <- raster::extract(
  rasta,
  sp::SpatialPoints(dplyr::select(df_sites, lon, lat) %>% distinct()),
  sp = TRUE
  ) %>%
  as_tibble() %>%
  rename(gti = GDAL.Band.Number.1) %>%
  right_join(df_sites, by = c("lon", "lat")) %>%
  relocate(name_site, lon, lat, cluster, gti) # change order

save(df_gti, file = "./df_gti.RData")


