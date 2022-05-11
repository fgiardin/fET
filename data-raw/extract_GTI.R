# Extract and read global topographic index (GTI)

library(tidyverse)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis

# load data and see structure
nc_data <- nc_open('~/data/gti_marthews/ga2_0_05deg.nc')
# Save the print(nc) dump to a text file
{
  sink('ga2_0_05deg_metadata.txt')
  print(nc_data)
  sink()
}

load("/Users/fgiardina/fvar/output/00_Figures_4/dataframes/table1_soil.RData")
df_sites <- table1 #%>% dplyr::select(name_site, lon, lat)

rasta <- raster::raster("~/data/gti_marthews/ga2_0_05deg.nc")
df_gti <- raster::extract(rasta, sp::SpatialPoints(dplyr::select(df_sites, lon, lat) %>% distinct()), sp = TRUE) %>%
  as_tibble() %>% 
  rename(gti = TOPMODEL.index) %>% 
  #rename(gti = GDAL.Band.Number.1) %>% 
  right_join(df_sites, by = c("lon", "lat")) %>% 
  relocate(name_site, lon, lat, cluster, gti) # change order


