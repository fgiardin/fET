# read Transpiration datasets and put in right format for ML model

devtools::load_all(".")
library(tidyverse)
library(data.table)
library(lubridate)
library(bigleaf)
library(LSD)

sitename = "US-MMS" # available for "US-Ton", "IT-Cpz", "AU-How", "DK-Sor", "US-MMS"

# load transpiration
raw_data <- fread(sprintf("data/Transpiration/%s.csv", sitename))

# adjust date
raw_data$date <- date_decimal(raw_data$year_dec, tz = "UTC") %>%
  ceiling_date(
    unit = "hours",
    change_on_boundary = NULL,
    week_start = getOption("lubridate.week.start", 7)
  )

# clean df
df_T <- raw_data %>%
  dplyr::select(date, T)

# load hhdf and ddf
hhdf_name = sprintf("data/output/%s/data_frames/hhdf_%s.RData", sitename, sitename)
ddf_name = sprintf("data/output/%s/data_frames/ddf_%s.RData", sitename, sitename)
load(ddf_name)
load(hhdf_name)

# merge transpiration
ddf_T <- df_T %>%
  dplyr::filter(T > 0.001) %>%
  dplyr::select(date, T) %>%
  mutate(date = lubridate::date(date)) %>%
  group_by(date) %>%
  summarise(T = sum(T, na.rm = TRUE)) %>%  # calculate daily sum
  ungroup()

# join back to daily df
ddf_T <- ddf %>%
  left_join(ddf_T, by = "date")

# check agreement between T and ET
file = "./scatter_ET-T.png"
scatterheat(ddf_T, "ET", "T", "Agreement T-ET", file)

# save
file1 = sprintf("data/output/%s/data_frames/ddf_T_%s.RData", sitename, sitename)
save(ddf_T, file = file1)






