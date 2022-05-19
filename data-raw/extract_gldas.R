#!/usr/bin/env Rscript

# extract needed variables from GLDAS world maps
# designed to run per site (consistent with rest of code)

# variables:
# Evap_tavg, (kg m-2 s-1)
# PotEvap_tavg, (W m-2)
# Rainf_tavg (rain precipitation) (kg m-2 s-1)
# Rainf_f_tavg (total precipitation)

## evaluate arguments (they are then available as args[1], args[2], ...
args = commandArgs(trailingOnly=TRUE)

# define sitename
site_ID = args[1] # don't call 'sitename' as in other scripts, otherwise it will
                  # generate conflicts with the column 'sitename' of the
                  # siteinfo_fluxnet2015 dataframe


devtools::load_all(".")
library(bigleaf)
library(tidyverse)
library(raster)
library(ncdf4)

# Given the size of the entire downloaded data (~2 GB), only a handful of
# world maps are provided here, to run the code locally as example.
# You can download the rest of the maps from the GLDAS website (good luck with
# that!)

# Specify path containing GLDAS maps.
path = "data-raw/GLDAS/"
layer = "PotEvap_tavg"
df_PET <- process_ldas(path, layer, site_ID)
df_PET <- df_PET %>%
  mutate(PET = values)

layer = "Evap_tavg"
df_ET <- process_ldas(path, layer, site_ID)
df_ET <- df_ET %>%
  mutate(ET = values)

layer = "Rainf_tavg"
df_P <- process_ldas(path, layer, site_ID)
df_P <- df_P %>%
  mutate(P = values)

# create one df with the three variables
df_gldas <- df_PET %>%
  dplyr::select(sitename, date, PET) %>%
  left_join(df_ET %>% dplyr::select(sitename, date, ET), by = c("date", "sitename")) %>%
  left_join(df_P %>% dplyr::select(sitename, date, P), by = c("date", "sitename"))

### save it
# define directories
dir.create("data/GLDAS") # create separate directory from where actual data is
                        # stored to avoid overwriting
gldas_path = sprintf("data/GLDAS/%s", site_ID)
dir.create(gldas_path)

# save
df_name = sprintf("%s/df_gldas_raw_%s.RData", gldas_path, site_ID)
save(df_gldas, file = df_name)


### CALCULATE CWD WITH GLDAS DATA ####
# calculate water balance
df_gldas <- df_gldas %>%
  mutate(water_balance = P - ET) %>% # both in (kg m-2 s-1)
  mutate(water_balance = water_balance*3*60*60) # convert to kg m-2 d-1

# calculate CWD BEFORE filters
ddf_CWD_gldas <- mct(
  df_gldas %>%
    dplyr::select(water_balance,date) %>%
    mutate(date = lubridate::date(date)) %>%  # needed to average at daily level
    group_by(date) %>% #group_by(date, hour) %>%
    summarise(
      water_balance = sum(water_balance, na.rm = TRUE), # SUM (altrimenti sono tipo valori istantanei)
    ) %>%
    na.omit(),
  "water_balance",
  "date"
)

# save CWD
CWD_name = sprintf("%s/ddf_CWD_gldas_%s.RData", gldas_path, site_ID)
save(ddf_CWD_gldas, file = CWD_name)


#### merge with fluxnet ddf ####
# extract temperature from fluxnet (needed for further analysis) and retain the same
# data points as fluxnet data after data screening (for consistency)

# load hhdf fluxnet
hhdf_name = sprintf("data/output/%s/data_frames/hhdf_%s.RData", site_ID, site_ID)
load(hhdf_name)

# attach temperature and keep only lines that appear in fluxnet df after filtering
df_gldas <- df_gldas %>%
  inner_join(hhdf %>% dplyr::select(date, TA_F), by = "date") %>% # select only dates that are in both (to make sure to take filtered data only)
  mutate(PET = LE.to.ET(PET,TA_F)) # from (W m-2) to (kg m-2 s-1)

# summarise daily
ddf_gldas <- df_gldas %>%
  mutate(date = lubridate::date(date)) %>%
  group_by(date) %>%
  mutate(P = P*3*60*60, # convert to mm/day
         ET = ET*3*60*60,
         PET = PET*3*60*60) %>%
  summarise(P = sum(P, na.rm = TRUE), # calculate daily sum (for P and ET)
            ET = sum(ET, na.rm = TRUE),
            PET = sum(PET, na.rm = TRUE),
            )


# calculate fET
ddf_gldas <- ddf_gldas %>%
  mutate(fvar = ET/PET)

df_name = sprintf("%s/ddf_gldas_%s.RData", gldas_path, site_ID)
save(ddf_gldas, file = df_name)








