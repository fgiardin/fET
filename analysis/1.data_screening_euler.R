#!/usr/bin/env Rscript

# The first two lines of code below are needed to run it on Euler. You can run it locally by
# manually defining the 'sitename' and skipping those two lines (use FLUXNET2015 nomenclature).

# This script automatically creates a directory for every site in data/output
# where output from this script and 2.run_ML_model_euler.R are saved.

## evaluate arguments (they are then available as args[1], args[2], ...
args = commandArgs(trailingOnly=TRUE)

# define sitename
sitename = args[1] # put equal to args(1)

#Load packages
devtools::load_all(".")
library(tidyverse)
library(caret)
library(bigleaf)
library(data.table)


# LOAD GAPFILLED DATASET FROM Stocker at al. 2018 -------------------------
# This dataset was gapfilled with single-layer neural networks and it is
# used here to calculate CWD only (see methods)

# load df
fluxnet <- readRDS("~/data/fLUE/modobs_fluxnet2015_s11_s12_s13_with_SWC_v3.rds")
# locally, the file linked above is here: data/modobs_fluxnet2015_s11_s12_s13_with_SWC_v3.rds

ddf_flue_raw <- fluxnet[[sitename]]

year <- ddf_flue_raw[["ddf"]][["s11"]][["year"]]
doy <- ddf_flue_raw[["ddf"]][["s11"]][["doy"]]
moy <- ddf_flue_raw[["ddf"]][["s11"]][["moy"]]
dom <- ddf_flue_raw[["ddf"]][["s11"]][["dom"]]
TA_F <- ddf_flue_raw[["ddf"]][["inp"]][["temp"]]
LE_F_MDS <- ddf_flue_raw[["ddf"]][["obs"]][["le_f_mds_mygapfilled"]]
P_F <- ddf_flue_raw[["ddf"]][["inp"]][["prec"]]
VPD_F <- ddf_flue_raw[["ddf"]][["inp"]][["vpd"]]
fAPAR_modis <- ddf_flue_raw[["ddf"]][["inp"]][["fpar"]]
EVI <- ddf_flue_raw[["ddf"]][["inp"]][["evi"]]
wcont_s11 <- ddf_flue_raw[["ddf"]][["s11"]][["wcont"]]
wcont_s12 <- ddf_flue_raw[["ddf"]][["s12"]][["wcont"]]
wcont_s13 <- ddf_flue_raw[["ddf"]][["s13"]][["wcont"]]
soilm_from_et <- ddf_flue_raw[["ddf"]][["swc_by_etobs"]][["soilm_from_et"]]
soilm_from_et_orthbucket <- ddf_flue_raw[["ddf"]][["swc_by_etobs"]][["soilm_from_et_orthbucket"]]
# for fLUE
GPP <- ddf_flue_raw[["ddf"]][["obs"]][["gpp_obs2015_GPP_NT_VUT_REF_gfd"]]
PPFD <- ddf_flue_raw[["ddf"]][["inp"]][["ppfd"]]

# create df
ddf_flue <- data.frame(
  year,
  doy,
  moy,
  dom,
  TA_F,
  LE_F_MDS,
  P_F,
  VPD_F,
  fAPAR_modis,
  EVI,
  wcont_s11,
  wcont_s12,
  wcont_s13,
  soilm_from_et,
  soilm_from_et_orthbucket,
  GPP,
  PPFD
)

# adjust date
ddf_flue$date <- paste(ddf_flue$year, ddf_flue$moy, ddf_flue$dom, sep="-") %>%
  lubridate::ymd()

# calculate ET
ddf_flue <- ddf_flue %>%
  mutate(ET = LE.to.ET(LE_F_MDS, TA_F)) %>% #get ET with bigleaf function (mm/s)
  mutate(water_balance = P_F - ET) %>%  # both P and ET in mm/s (absolute in 30 minutes)
  dplyr::select(-year,-doy,-moy,-dom)

# calculate CWD with gapfilled data and save it
ddf_CWD <- mct(
  ddf_flue %>%
    dplyr::select(water_balance,date) %>%
    mutate(date = lubridate::date(date)) %>%
    group_by(date) %>%
    summarise(
      water_balance = sum(water_balance, na.rm = TRUE),
    ) %>%
    na.omit(),
  "water_balance",
  "date"
)

# create directory with site name to save all data
dir_name = sprintf("data/output/%s", sitename)
dir.create(dir_name)

# create subdirectories
data_frames_path = sprintf("%s/data_frames", dir_name)
TS_path = sprintf("%s/TS", dir_name)
dir.create(data_frames_path)
dir.create(TS_path)

# save df flue
flue_name = sprintf("%s/ddf_flue_%s.RData", data_frames_path, sitename)
save(ddf_flue, file = flue_name)

# save CWD file
CWD_name = sprintf("%s/data_frames/ddf_CWD_%s.RData", dir_name, sitename)
save(ddf_CWD, file = CWD_name)



# LOAD RAW DATA FROM FLUXNET AND PROCESS IT AS IN LI et al. 2018 ----------

# The directory ~/data/FLUXNET-2015_Tier1/20191024/HH must contain all original
# half-hourly FLUXNET2015 .CSV files as available on https://fluxnet.org/data/fluxnet2015-dataset/

# If running locally, you can try setting filename = "AU-Wom" and load the .csv provided
# here as example: /data-raw/FLX_AU-Wom_FLUXNET2015_FULLSET_HH_2010-2012_1-3.csv
# In this case, directly read the commented local path below (line 135)

# extract list of files in directory
file_list <- list.files(path="~/data/FLUXNET-2015_Tier1/20191024/HH") # path

# find the name of the file which contains the site name
grepsite <- sprintf("%s_FLUXNET2015_FULLSET", sitename)
filename <- grep(grepsite, file_list, value=TRUE)

# read it
csv_path <- sprintf("~/data/FLUXNET-2015_Tier1/20191024/HH/%s", filename)
#csv_path <- sprintf("data-raw/FLX_AU-Wom_FLUXNET2015_FULLSET_HH_2010-2012_1-3.csv") # local path

raw_dataHH <- fread(csv_path)

# load and process data
hhdf <- raw_dataHH %>%
  # select only relevant data
  dplyr::select(
    one_of( #one_of only gives a warning if the column doesn't exist (without it, it gives error that blocks execution)
      "TIMESTAMP_START",
      "TIMESTAMP_END",
      "TA_F", #temperature
      "TA_F_QC",
      "NETRAD", #Net radiation
      "WS_F", #windspeed
      "WS_F_QC",
      "LE_F_MDS", #latent heat
      "LE_F_MDS_QC",
      "P_F",
      "P_F_QC",
      "USTAR", #friction velocity
      "VPD_F", # VPD
      "VPD_F_QC",
      "H_CORR", # Sensible heat flux, W/m2
      "H_F_MDS",
      "H_F_MDS_QC",
      "GPP_NT_VUT_REF", # GPP
      "NEE_VUT_REF_QC", # 0 = measured; 1 = good quality gapfill; 2 = medium; 3 = poor
      "SW_IN_POT", #incoming shortwave radiation at the top of atm
      "RH",
      "G_F_MDS",
      "G_F_MDS_QC" # soil heat flux
    ),
    starts_with("SWC_F_MDS"), #soil moisture (all layers)
    starts_with("TS_F_MDS"), #soil temperature (all layers)
  ) %>%
  # time conversion
  mutate(TIMESTAMP_START = lubridate::ymd_hm(TIMESTAMP_START)) %>%
  mutate(TIMESTAMP_END = lubridate::ymd_hm(TIMESTAMP_END)) %>%
  rename(date = TIMESTAMP_END) %>%
  na_if(-9999)   # correctly represent missing data

# choose H variable
if (all(is.na(hhdf$H_CORR))) {
  hhdf <- hhdf %>%
    mutate(H = H_F_MDS) %>%
    dplyr::select(-H_F_MDS,-H_CORR)
} else {
  hhdf <- hhdf %>%
    mutate(H = H_CORR) %>%  # if available, use H_CORR
    dplyr::select(-H_F_MDS,-H_CORR)
}

# calculate RH if not available
RH_flag = "RH" %in% names(raw_dataHH)

if (!RH_flag) {
  hhdf <- hhdf %>%
    mutate(
      VPD_F = ifelse(VPD_F_QC %in% c(0,1), VPD_F, NA),
      TA_F = ifelse(TA_F_QC %in% c(0,1), TA_F, NA),
      RH = 100*(1-VPD_F/(0.6108*exp((17.27*TA)/(TA+237.3))))
    )
}

## filter data based on quality check
# 0 = measured; 1 = good quality gapfill; 2 = medium; 3 = poor

# first handle multi-layered variables
for (i in 1:10){
  SWC = sprintf("SWC_F_MDS_%d", i)
  SWC_QC = sprintf("SWC_F_MDS_%d_QC", i)
  flag_layer = SWC %in% names(raw_dataHH) && SWC_QC %in% names(raw_dataHH)
  if (flag_layer) {
    hhdf <- hhdf %>%
      mutate(
        SWC = ifelse(SWC_QC %in% c(0,1), SWC, NA),
      )
  }
}

for (i in 1:10){
  TS = sprintf("TS_F_MDS_%d", i)
  TS_QC = sprintf("TS_F_MDS_%d_QC", i)
  flag_layer = TS %in% names(raw_dataHH) && TS_QC %in% names(raw_dataHH)
  if (flag_layer) {
    hhdf <- hhdf %>%
      mutate(
        TS = ifelse(TS_QC %in% c(0,1), TS, NA),
      )
  }
}

hhdf <- hhdf %>%
  mutate(
    TA_F = ifelse(TA_F_QC %in% c(0,1), TA_F, NA),
    WS_F = ifelse(WS_F_QC %in% c(0,1), WS_F, NA),
    LE_F_MDS = ifelse(LE_F_MDS_QC %in% c(0,1), LE_F_MDS, NA),
    P_F = ifelse(P_F_QC %in% c(0,1), P_F, NA),
    VPD_F = ifelse(VPD_F_QC %in% c(0,1), VPD_F, NA),
    H = ifelse(H_F_MDS_QC %in% c(0,1), H, NA),
    GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC %in% c(0,1), GPP_NT_VUT_REF, NA),
    G_F_MDS = ifelse(G_F_MDS_QC %in% c(0,1), G_F_MDS, NA)
    #SW_IN = ifelse(SW_IN_QC %in% c(0,1), SW_IN, NA)
    # NETRAD = ifelse(NETRAD_QC %in% c(0,1), NETRAD, NA),
    # USTAR = ifelse(USTAR_QC %in% c(0,1), USTAR, NA)
  ) %>%

  # erase QC variables
  dplyr::select(-ends_with("_QC")) %>%

  # calculate ET (with quality checked data)
  mutate(ET = LE.to.ET(LE_F_MDS, TA_F)) %>% #get ET with bigleaf function (mm/s)
  mutate(ET = ET*30*60) # convert to absolute mm in 30min (like P_F)

# Note: using the final screened dataframe to calculate CWD would give erroneous results.
# We are removing all rain events, so IAV of P and ET won't be reliable.

# DEFINE RAIN FILTER
# exclude 6 hours after rain event; threshold: 0mm
hhdf <- hhdf %>% mutate(precip_flag = P_F)
for (i in 1:nrow(hhdf)) {
  precip_day <- hhdf$precip_flag[i]
  if (is.na(precip_day)) {
    next
  } else if(precip_day > 0){
     if(i >= nrow(hhdf)-12){
       hhdf$precip_flag[i:nrow(hhdf)] <- NA
     } else{
         hhdf$precip_flag[i:(i+12)] <- NA # 12 half-hours = 6 hours
       }
  }
}

#REMOVE DATA (as in Xi Li et al. 2018, only exception: they use 9-17h filter for day time, we filter on GPP)
hhdf <- hhdf %>%
  dplyr::filter(!is.na(precip_flag)) %>%  # take only rows where precip_flag is not NA (filter defined above)
  dplyr::filter(GPP_NT_VUT_REF > 0.0) %>% # day-time only
  dplyr::filter(ET > 0) %>%
  dplyr::filter(VPD_F > 0) %>%
  dplyr::filter(LE_F_MDS > 5) %>%
  dplyr::filter(SW_IN_POT > 50) %>%
  dplyr::filter(RH < quantile(RH, 0.95, na.rm = TRUE)) %>%  # get rid of RH higher than 95% quantile
  dplyr::select(-precip_flag) # remove useless columns

### KEEP ONLY DAYS WITH AT LEAST 8 SINGLE MEASUREMENT POINTS ###
# To have daily mean calculated with significant number of points
hhdf <- hhdf %>%
  mutate(day = lubridate::date(date)) %>%
  group_by(day) %>%
  filter(n() > 7) %>%
  ungroup() %>%
  dplyr::select(-day)

# aggregate to daily (nested format, so that it works even if SWC is not provided)
ddf <- inner_join(hhdf %>%
                    mutate(date = lubridate::date(date)) %>%
                    group_by(date) %>%
                    summarise_if(is.numeric, mean, na.rm = TRUE) %>% # calculate daily MEAN for most variables
                    dplyr::select(-P_F, -ET, -VPD_F), # remove columns where I don't calculate the mean
                  inner_join(
                    hhdf %>%
                      mutate(date = lubridate::date(date)) %>%
                      dplyr::select(date, P_F, ET) %>%
                      group_by(date) %>%
                      summarise(P_F = sum(P_F, na.rm = TRUE), # calculate daily sum (for P and ET)
                                ET = sum(ET, na.rm = TRUE)),
                    hhdf %>%
                      mutate(date = lubridate::date(date)) %>%
                      dplyr::select(date, VPD_F) %>% # calculate daily max for VPD
                      group_by(date) %>%
                      summarise(VPD_F = max(VPD_F, na.rm = TRUE)),
                  by = c('date')),
      by = c('date')) %>%
  ungroup()

# calculate EF (imp: after calculating daily LE and NETRAD daily means - otherwise noisy)
ddf <- ddf %>%
  mutate(EF = LE_F_MDS/NETRAD) # G_F_MDS ~ 0 if daily sum

# append modelled soil moisture and EVI from Stocker et al. 2018
ddf_flue = ddf_flue %>%
  dplyr::select(date, EVI, fAPAR_modis, wcont_s11, wcont_s12, wcont_s13, soilm_from_et, soilm_from_et_orthbucket)
ddf = ddf %>%
  left_join(ddf_flue, by = "date")

# save everything
file1 = sprintf("%s/data_frames/hhdf_%s.RData", dir_name, sitename)
file2 = sprintf("%s/data_frames/ddf_%s.RData", dir_name, sitename)
save(hhdf, file = file1)
save(ddf, file = file2)



# PRINT TIMESERIES OF ENVIRONMENTAL VARIABLES -----------------------------
# Used to check individual sites in analysis -- not provided here (for storage reasons)

# Visualize missing data
library(visdat)
ddf %>%
  vis_miss(   # DON'T use sample_n with visdat, it shuffles the data randomly
    cluster = FALSE,
    sort_miss = TRUE
  )
ggsave("missing_data.png", path = TS_path, width = 5, height = 3)


#ET
if ("ET" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=ET)) +
    geom_line() +
    labs(x = "Time", y = "ET")
  ggsave("ET_timeseries_daily.png", path = TS_path, width = 5, height = 3)
    }

# soil moisture
if ("SWC_F_MDS_1" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=SWC_F_MDS_1)) +
    geom_line() +
    labs(x = "Time", y = "SWC_F_MDS_1")
  ggsave("SM_timeseries.png", path = TS_path, width = 5, height = 3)
}

# fpar
if ("fAPAR_modis" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=fAPAR_modis)) +
    geom_line() +
    labs(x = "Time", y = "fAPAR")
  ggsave("fpar_timeseries.png", path = TS_path, width = 5, height = 3)
}

# EVI
if ("EVI" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=EVI)) +
    geom_line() +
    labs(x = "Time", y = "EVI")
  ggsave("EVI_timeseries.png", path = TS_path, width = 5, height = 3)
}


# VPD
if ("VPD_F" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=VPD_F)) +
    geom_line() +
    labs(x = "Time", y = "VPD")
  ggsave("VPD_F_timeseries.png", path = TS_path, width = 5, height = 3)
}

# Latent Heat
if ("LE_F_MDS" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=LE_F_MDS)) +
    geom_line() +
    labs(x = "Time", y = "latent heat")
  ggsave("LE_timeseries.png", path = TS_path, width = 5, height = 3)
}

#NETRAD
if ("NETRAD" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=NETRAD)) +
    geom_line() +
    labs(x = "Time", y = "netrad")
  ggsave("netrad_timeseries.png", path = TS_path, width = 5, height = 3)
}

# EF
if ("EF" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=EF)) +
    geom_line() +
    labs(x = "Time", y = "EF") +
    ylim(-1, 1)
  ggsave("EF_timeseries.png", path = TS_path, width = 5, height = 3)
}

#temperature
if ("TA_F" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=TA_F)) +
    geom_line() +
    labs(x = "Time", y = "temp")
  ggsave("T_timeseries.png", path = TS_path, width = 5, height = 3)
}

#WS
if ("WS_F" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=WS_F)) +
    geom_line() +
    labs(x = "Time", y = "Wind Speed")
    ggsave("WS_timeseries.png", path = TS_path, width = 5, height = 3)
}

#ustar
if ("USTAR" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=USTAR)) +
    geom_line() +
    labs(x = "Time", y = "Friction Velocity")
    ggsave("ustar_timeseries.png", path = TS_path, width = 5, height = 3)
}

#Soil T
if ("TS_F_MDS_1" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=TS_F_MDS_1)) +
    geom_line() +
    labs(x = "Time", y = "Soil Temperature")
    ggsave("soilT_timeseries.png", path = TS_path, width = 5, height = 3)
}

#precip
if ("P_F" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=P_F)) +
    geom_line() +
    labs(x = "Time", y = "Precipitation")
    ggsave("precip_timeseries.png", path = TS_path, width = 5, height = 3)
}

#GPP
if ("GPP_NT_VUT_REF" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=GPP_NT_VUT_REF)) +
    geom_line() +
    labs(x = "Time", y = "GPP")
    ggsave("GPP_timeseries.png", path = TS_path, width = 5, height = 3)
}

# CWD
if ("deficit" %in% names(ddf_CWD$df)) {
  ggplot(data = ddf_CWD$df, aes(x=date, y=deficit)) +
    geom_line() +
    labs(x = "Time", y = "CWD")
    ggsave("CWD_timeseries.png", path = TS_path, width = 5, height = 3)
}

# incoming SW
if ("SW_IN_POT" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=SW_IN_POT)) +
    geom_line() +
    labs(x = "Time", y = "SW_IN_POT")
    ggsave("SW_timeseries.png", path = TS_path, width = 5, height = 3)
}

# modelled SM
if ("wcont_s11" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=wcont_s11)) +
    geom_line() +
    labs(x = "Time", y = "wcont_s11")
    ggsave("wcont_s11_timeseries.png", path = TS_path, width = 5, height = 3)
}

if ("wcont_s12" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=wcont_s12)) +
    geom_line() +
    labs(x = "Time", y = "wcont_s12")
    ggsave("wcont_s12_timeseries.png", path = TS_path, width = 5, height = 3)
}

if ("wcont_s13" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=wcont_s13)) +
    geom_line() +
    labs(x = "Time", y = "wcont_s13")
    ggsave("wcont_s13_timeseries.png", path = TS_path, width = 5, height = 3)
}

if ("soilm_from_et" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=soilm_from_et)) +
    geom_line() +
    labs(x = "Time", y = "soilm_from_et")
    ggsave("soilm_from_et_timeseries.png", path = TS_path, width = 5, height = 3)
}

if ("soilm_from_et_orthbucket" %in% names(ddf)) {
  ggplot(data = ddf, aes(x=date, y=soilm_from_et_orthbucket)) +
    geom_line() +
    labs(x = "Time", y = "soilm_from_et_orthbucket")
    ggsave("soilm_from_et_orthbucket_timeseries.png", path = TS_path, width = 5, height = 3)
}

