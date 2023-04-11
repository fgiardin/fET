# read Transpiration datasets and put in right format for ML model

library(tidyverse)
library(data.table)
library(lubridate)

sitename = "FR-Pue" # available for "US-Ton", "IT-Cpz", "AU-How", "DK-Sor"

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
ddf_T <- hhdf %>%
  rename(date_end = date) %>%
  rename(date = TIMESTAMP_START) %>%
  left_join(df_T, by = "date") %>%
  dplyr::select(date, T) %>%
  mutate(date = lubridate::date(date)) %>%
  group_by(date) %>%
  summarise(T = sum(T, na.rm = TRUE)) %>%  # calculate daily sum
  ungroup()

# join back to daily df
ddf_T <- ddf %>%
  left_join(ddf_T, by = "date")

# save
file1 = sprintf("data/output/%s/data_frames/ddf_T_%s.RData", sitename, sitename)
save(ddf_T, file = file1)






