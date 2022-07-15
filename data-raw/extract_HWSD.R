#### extract HWSD data (it takes some minutes to run) #######################

# load packages
library(hwsdr) # Koen Hufkens's package to download HWSD database // #to see available data do: hwsd_meta_data
devtools::load_all(".")
library(tidyverse)

# load table1 with list of sites (used for indexing loop below)
load("manuscript/Figures/dataframes/table1_raw.RData")

# initialize empty dataframe
soil_variables <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("AWC_CLASS","T_CLAY", "S_CLAY", "T_SAND", "S_SAND", "T_SILT", "S_SILT"))
# it takes time to run!
for (i in 1:nrow(table1)){
  print(i)

  variables_site <- ws_subset(  # LAT, LON e non LON, LAT!!!
    site = "HWSD",
    location = c(table1$lat[i], table1$lon[i]),
    param = c("AWC_CLASS","T_CLAY", "S_CLAY", "T_SAND", "S_SAND", "T_SILT", "S_SILT")
  )

  variables_site <- variables_site %>%
    dplyr::select(-site, -latitude, -longitude) %>%  # menzionare in methods di arrotondamento lon e lat
    pivot_wider(names_from = parameter, values_from = value)

  soil_variables <- rbind(soil_variables, variables_site)
  }

# add to table
table1 <- cbind(table1, soil_variables)
save(table1, file = "./table1_soil.RData")  # save it in main directory to avoid overwriting good table
