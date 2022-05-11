#!/usr/bin/env Rscript

# extract and put GLDAS product in right format
# designed to run per site (consistent with rest of code)

# variables: 
# Evap_tavg, (kg m-2 s-1)
# PotEvap_tavg, (W m-2)
# Rainf_tavg (rain precipitation) (kg m-2 s-1)
# Rainf_f_tavg (total precipitation)

## evaluate arguments (they are then available as args[1], args[2], ...
args = commandArgs(trailingOnly=TRUE)

# define sitename
site_ID = args[1] # non lo chiamo sitename perchè senno genera conflict con la colonna del siteinfo_fluxnet2015 con lo stesso nome 

devtools::load_all(".")
library(bigleaf)
library(tidyverse)
library(raster)
library(ncdf4)

# uncomment these lines to actually extract data (otherwise will just load it)
# path = "~/data/LDAS/GLDAS/"
# layer = "PotEvap_tavg"
# df_PET <- process_ldas(path, layer, site_ID) 
# df_PET <- df_PET %>% 
#   mutate(PET = values) 
# 
# layer = "Evap_tavg"
# df_ET <- process_ldas(path, layer, site_ID)
# df_ET <- df_ET %>% 
#   mutate(ET = values) 
# 
# layer = "Rainf_tavg"
# df_P <- process_ldas(path, layer, site_ID)
# df_P <- df_P %>% 
#   mutate(P = values)
# 
# # one df with the three variables that I need
# df_gldas <- df_PET %>% 
#   dplyr::select(sitename, date, PET) %>% 
#   left_join(df_ET %>% dplyr::select(sitename, date, ET), by = c("date", "sitename")) %>% 
#   left_join(df_P %>% dplyr::select(sitename, date, P), by = c("date", "sitename"))

### save it
# define directories
dir_name = sprintf("./output/%s", site_ID) # path to directory of site (in teoria gia creata in pre_process)
dir.create(dir_name)
# create gldas path
gldas_path = sprintf("%s/GLDAS", dir_name)
dir.create(gldas_path)
# save
df_name = sprintf("%s/df_gldas_raw_%s.RData", gldas_path, site_ID)
load(df_name)
# save(df_gldas, file = df_name) 

# merge with fluxnet ddf
# devo prendere temperatura e applicare same filtering (in teoria easy, basta prendere hhdf e tenere solo i punti temporali che compaiono in quel dataframe dopo aver applicato i filtri)

# load hhdf fluxnet 
########## USE GAPFILLED DATA? directly use CWD daily from final dataset e usare HH dataset per i filtri

hhdf_name = sprintf("%s/data_frames/hhdf_%s.RData", dir_name, site_ID)
load(hhdf_name)

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
########################################################################
### PRINT TIMESERIES OF ENVIRONMENTAL VARIABLES  #######################
########################################################################
if ("deficit" %in% names(ddf_CWD_gldas$df)) {
  ggplot(data = ddf_CWD_gldas$df, aes(x=date, y=deficit)) +
    geom_line() +
    labs(x = "Time", y = "CWD") 
  ggsave("CWD_timeseries_gldas.png", path = gldas_path, width = 5, height = 3)
}

if ("P" %in% names(ddf_gldas)) {
  ggplot(data = ddf_gldas, aes(x=date, y=P)) +
    geom_line() +
    labs(x = "Time", y = "Precipitation") 
  ggsave("precip_timeseries.png", path = gldas_path, width = 5, height = 3)
}


if ("ET" %in% names(ddf_gldas)) {
  ggplot(data = ddf_gldas, aes(x=date, y=ET)) +
    geom_line() +
    labs(x = "Time", y = "ET") 
  ggsave("ET_timeseries.png", path = gldas_path, width = 5, height = 3)
}

if ("PET" %in% names(ddf_gldas)) {
  ggplot(data = ddf_gldas, aes(x=date, y=PET)) +
    geom_line() +
    labs(x = "Time", y = "PET") 
  ggsave("PET_timeseries.png", path = gldas_path, width = 5, height = 3)
}

##### TIME SERIES OF FVAR #####
m <- ggplot(data = ddf_gldas, aes(x=date)) +
  geom_line(aes(y = ET, color = "ET")) +
  geom_line(aes(y = PET, color = "PET")) +
  labs(x = "Time", y = "mm/day", color = "Legend") 
#ylim(min(out$df_all$nn_pot), max(out$df_all$nn_pot))
m +  scale_color_manual(values = c("chartreuse","brown1"))
# save plot
ggsave("ET_vs_PET.png", path = gldas_path, width = 10, height = 3)

# ZOOM
m <- ggplot(data = ddf_gldas %>% slice(100:500), aes(x=date)) +
  geom_line(aes(y = ET, color = "ET")) +
  geom_line(aes(y = PET, color = "PET")) +
  labs(x = "Time", y = "mm/day", color = "Legend") 
m +  scale_color_manual(values = c("chartreuse","brown1"))
# save plot
ggsave("ET_vs_PET_zoom.png", path = gldas_path, width = 5, height = 3)

########################################################################
### PRINT RESULTS ######################################################
########################################################################
# libraries
library(ggplot2)
library(LSD) # for density plot

# merge CWD to results df
ddf_plot <- ddf_gldas %>% 
  left_join(ddf_CWD_gldas$df, by = "date")

##### DENSITY PLOT fET vs CWD #####
### all data
rows=nrow(ddf_plot)
title = sprintf("%s, N = %d", site_ID, rows)

file = sprintf("%s/fET_vs_CWD_density_gldas.png", gldas_path)
png(filename = file, width = 4, height = 4.3, units = 'in', res = 300)
plot.new() 
plot.window(xlim = c(0,300),
#plot.window(xlim = c(min(ddf_plot$deficit, na.rm=TRUE),max(ddf_plot$deficit, na.rm=TRUE)),
            ylim = c(0,1.5))
heatscatterpoints(x=ddf_plot$deficit, y = ddf_plot$fvar)
axis(1)
axis(2)
title(main = title, xlab = "Cumulative Water Deficit (mm)", ylab = "fET")
box()
dev.off()

### only take big CWD instances
biginstances <- ddf_CWD_gldas$inst %>% 
  mutate(year = lubridate::year(date_start)) %>% 
  group_by(year) %>% 
  dplyr::filter(deficit == max(deficit)) %>% 
  pull(iinst)

ddf_plot_biginstances <- ddf_plot %>% 
  dplyr::filter(iinst %in% biginstances)

rows=nrow(ddf_plot_biginstances)
title = sprintf("%s, N = %d", site_ID, rows)

file = sprintf("%s/fET_vs_CWD_density_biginstances_gldas.png", gldas_path)
png(filename = file, width = 4, height = 4.3, units = 'in', res = 300)
plot.new() 
plot.window(xlim = c(0,300),
#plot.window(xlim = c(min(ddf_plot_biginstances$deficit, na.rm=TRUE),max(ddf_plot_biginstances$deficit, na.rm=TRUE)),
            ylim = c(0,1.5))
heatscatterpoints(x=ddf_plot_biginstances$deficit, y = ddf_plot_biginstances$fvar)
axis(1)
axis(2)
title(main = title, xlab = "Big instances CWD (mm)", ylab = "fET")
box()
dev.off()

### only take big CWD instances + normalize fET by value in lowest CWD bin
# normalize fET per median of value with no stress 
fET_ddf_median = ddf_plot %>% 
  dplyr::filter(deficit < 20) 
Median_fET = median(fET_ddf_median$fvar, na.rm = TRUE)

ddf_plot = ddf_plot %>% 
  mutate(fvar_normalized = fvar/Median_fET)

# density plot
rows=nrow(ddf_plot)
title = sprintf("%s, N = %d", site_ID, rows)

file = sprintf("%s/fET_vs_CWD_density_biginstances_gldas_normalised.png", gldas_path)
png(filename = file, width = 4, height = 4.3, units = 'in', res = 300)
plot.new() 
plot.window(xlim = c(0, 300),
#plot.window(xlim = c(min(ddf_plot$deficit, na.rm=TRUE),max(ddf_plot$deficit, na.rm=TRUE)),
            ylim = c(0,1.5))
heatscatterpoints(x=ddf_plot$deficit, y = ddf_plot$fvar_normalized)
axis(1)
axis(2)
title(main = title, xlab = "Big instances CWD (mm)", ylab = "normalized fET")
#mtext(side=0.5, line=6, at=1, adj=0, cex=0.7, col = 'black', subtitle)
box()
dev.off()



        # ##### BILINEAR REGRESSION #####
# library(segmented)
# #put data in right format for calc_cwd function
# ddf_CWD_fvar <- ddf_CWD_gldas$df %>% 
#   left_join(ddf_plot %>% dplyr::select(fvar,date), by = "date") 
# 
# segmented <- fvar::calc_cwd_lue0(ddf_CWD_fvar, ddf_CWD_gldas$inst, "fvar", do_plot = TRUE)
# file = sprintf("%s/segmented_gldas_%s.RData", gldas_path, site_ID)
# save(segmented, file = file)
# 
# segmented$gg +
#   labs(title = sprintf("%s, Number of breakpoints = %d", site_ID, segmented$num_splits)) +
#   ylab("fET") 
# ggsave("fET_vs_CWD_bilinear_gldas.png", path = gldas_path, width = 4, height = 4)




