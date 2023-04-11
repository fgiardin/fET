### run model for every site and plot metrics and results

#!/usr/bin/env Rscript

# ## evaluate arguments (they are then available as args[1], args[2], ...
# args = commandArgs(trailingOnly=TRUE)
#
# # define sitename
# sitename = args[1] # you can run locally and set sitename equal to a fluxnet site name (e.g. AU-Wom)
# print(sitename)
for i in c("US-Ton", "IT-Cpz", "AU-How", "DK-Sor"){
sitename = i

#Load packages
devtools::load_all(".")
library(tidyverse)
library(caret)

# Import TensorFlow and the TensorBoard HParams plugin
library(tensorflow)
library(reticulate)
# use_condaenv("r-reticulate")
library(keras)
is_keras_available()
hp <- import("tensorboard.plugins.hparams.api")
library(tfruns)
library(tfestimators)

library(bigleaf)
library(yardstick)
library(ggpointdensity)
library(viridis)
library(LSD)

# create directory for results
dir_name = sprintf("data/output/%s", sitename) # path to directory of site (gia creata in pre_process)
data_frames_path = sprintf("%s/data_frames", dir_name) # path to dir of dataframes (gia creata)
results_path = sprintf("%s/results", dir_name)

# create results path
dir.create(dir_name)
dir.create(results_path)
dir.create(data_frames_path)

# load dataframes
# CWD + DDF + transpiration
CWD_name = sprintf("%s/data_frames/ddf_CWD_%s.RData", dir_name, sitename)
# ddf_name = sprintf("%s/data_frames/ddf_%s.RData", dir_name, sitename)
ddf_T_name = sprintf("%s/data_frames/ddf_T_%s.RData", dir_name, sitename)
load(CWD_name)
# load(ddf_name)
load(ddf_T_name)
ddf <- ddf_T


### DEFINE SETTINGS, FIND THRESHOLD AND RUN MODEL ######################

# define settings
settings <- list(
  target         = "T",
  predictors = c("NETRAD","VPD_F", "TA_F", "EVI"),
  varnams_soilm  = "SWC_F_MDS_1",
  rowid          = "date",
  threshold      = 0.5, # provisional soil moisture threshold
  package        = "keras", # "nnet" or "keras". nnet builds a single-layer neural networks
  print_model_summary = TRUE,
  nrep          = 5,
  nfolds        = 5, # number of cross-validation folds
  num_units_per_layer = c(8, 8, 8, 8, 8), #number of nodes per layer
                                          #for nnet: only first element of the vector is relevant
  ## KERAS specific settings (useless with nnet)
  batch_size          = 64,
  num_epochs          = 30,
  val_size            = 0.25,
  learning_rate       = 0.01,
  num_layers          = 2 # number of hidden layers (only for keras)
)


# USE MODELLED Soil Moisture IF SWC IS NOT CONSISTENT
# load .csv with soil moisture data classified according to their usability (see Methods)
soil_usability <- read.csv("data/soilm_data_usability_fluxnet2015.csv")

soil_usability_filtered <- soil_usability %>%
  dplyr::filter(mysitename == sitename)

if (soil_usability_filtered$code == 1) {
  settings$varnams_soilm = "SWC_F_MDS_1"
} else {
  settings$varnams_soilm = "wcont_s11"
}

# ###*** find optimal SM threshold ***###
# extract optimal SM treshold from Stocker et al. 2018
SM_thresholds_allsites <- readRDS("data/SM_thresholds_allsites.rds")

if(settings$varnams_soilm == "SWC_F_MDS_1") {
  settings$threshold = SM_thresholds_allsites %>%
    dplyr::filter(name_of_site == sitename) %>%
    dplyr::select(soilm_obs) %>%
    pull
} else if(settings$varnams_soilm == "wcont_s11") {
  settings$threshold = SM_thresholds_allsites %>%
    dplyr::filter(name_of_site == sitename) %>%
    dplyr::select(soilm_splash220) %>% # simulation s11 has a water holding capacity of 220 mm
    pull
}


# prepare data with correct threshold
df_train <- prepare_trainingdata_fvar(ddf, settings)

# # histogram of SM threshold (for Reviews)
# settings$varnams_soilm
# a <- ggplot(df_train, aes(x=wcont_s11)) +
#   geom_histogram(color="black", fill="grey45") +
#   theme_classic() +
#   geom_vline(aes(xintercept=settings$threshold),
#              color="blue", linetype="dashed", size=1) +
#   labs(
#     title = sitename,
#     x = "Soil moisture (%)",
#     y = "Frequency (d)") +
#   theme(plot.title = element_text(hjust = 0.5))
# a
# ggsave(paste0("SM_hist_", sitename, ".png"), path = "./", width = 5, height = 4)


###*** HP TUNING ***####
print("LAUNCH HP TUNING")
# # create a subdirectory for every site (to avoid run directories with same name updated at the same time)
# runs_path = sprintf("/cluster/scratch/fgiardina/%s", sitename)
# dir.create(runs_path)
# runs <- tuning_run("./R2/keras_grid_search.R",  # See methods for how we defined the tuning
#                    flags = list(
#                      nodes1 = c(8, 16, 32, 64),
#                      nodes2 = c(8, 16, 32, 64),
#                      nodes3 = c(8, 16, 32, 64),
#                      nodes4 = c(8, 16, 32, 64),
#                      nodes5 = c(8, 16, 32, 64),
#                      num_layers = c(1, 2, 3, 4, 5),
#                      optimizer = c("adam"),
#                      activation = c("relu"),
#                      batch_size = c(16, 32, 64),
#                      epochs = c(10, 20, 30),
#                      learning_rate = c(0.01)
#                    ),
#                    sample = 0.05, # pervarcentage of the total models to assess: 0.05; faster: 0.00005
#                    runs_dir = runs_path # don't save output directly in /runs (otherwise the high number of generated log files will clog Euler)
# )
#
# # save output
# file = sprintf("%s/runs_%s.RData", data_frames_path, sitename)
# save(runs, file = file)
# print("HP TUNING COMPLETED")
#
# # delete all logfiles of runs (otherwise there will be too many saved files and Euler will explode)
# clean_runs() # first archive them (otherwise won't delete)
# purge_runs() # then delete them

# directly load tuned HP
HP_name = sprintf("%s/data_frames/runs_%s.RData", dir_name, sitename)
load(HP_name)

# choose best run
best_run = runs %>%
  arrange(metric_val_loss) %>%
  slice(1:5) %>% # choose the best 5 according to validation loss
  # among the best 5, take the least complex model:
  mutate(        # isolate the number of parameters from printed output
    Tot_param = sub(
    ".*Trainable params: (.*)\nNon-trainable params: 0\n________________________________________________________________________________\n\n",
    "\\1",
    model)
    ) %>%
  mutate(       # correct for commas (if num of parameters > 1000, in the output will be printed as 1,000)
    Tot_param = as.numeric(gsub(",", "", Tot_param))
         ) %>%
  arrange(Tot_param) %>%
  slice(1)

# update settings with tuned model
settings$num_layers = best_run$flag_num_layers
settings$num_units_per_layer = c(best_run$flag_nodes1, best_run$flag_nodes2, best_run$flag_nodes3, best_run$flag_nodes4, best_run$flag_nodes5)
settings$batch_size = best_run$flag_batch_size
settings$num_epochs = best_run$flag_epochs

# Repeat algorithm with optimal SM threshold and tuned HPs
df_train <- prepare_trainingdata_fvar(ddf, settings)
out <- train_predict_fvar(
  df_train,
  settings,
  soilm_threshold    = settings$threshold,
  weights            = NA,
  verbose = TRUE
)

# save output
file = sprintf("%s/out_%s.RData", data_frames_path, sitename)
save(out, file = file)


### PRINT PERFORMANCE METRICS ##########################################

# These figures are not used in the final paper. They were just printed in the analysis
# to monitor the performance of the model.

print("PRINT PERFORMANCE METRICS")
##### TIME SERIES OF FVAR #####
print("TIME SERIES OF FVAR")
# append NETRAD
plottin = out$df_all %>%
  left_join(ddf %>% dplyr::select(date, TA_F, NETRAD), by ="date") %>%
  mutate(NETRAD = LE.to.ET(NETRAD, TA_F)*24*60*60) # convert NETRAD in same units of ET (kg/m2*d)
  #left_join(ddf %>% dplyr::select(date, TA_F, PPFD, EVI), by ="date")

# # get coeff to scale netrad
lm_model = lm(nn_pot ~ NETRAD + 0, plottin)
coeff = lm_model[["coefficients"]][["NETRAD"]]

m <- ggplot(data = plottin, aes(x=date)) +
  geom_line(aes(y = NETRAD*coeff, color = "NETRAD")) +
  geom_line(aes(y = obs, color = "obs")) + #*PPFD*EVI
  geom_line(aes(y = nn_act, color = "nn_act")) +
  geom_line(aes(y = nn_pot, color = "nn_pot")) +
  labs(x = "Time", y = "mm/day", color = "Legend") +
  ylim(0, 7.5) +
  theme_classic()
#ylim(min(out$df_cv$nn_pot), max(out$df_cv$nn_pot))
m +  scale_color_manual(values = c("black", "chartreuse","brown1","cornflowerblue"))
# save plot
ggsave("ET_vs_nn.png", path = results_path, width = 10, height = 3)

# ZOOM
m <- ggplot(data = plottin %>% slice(100:500), aes(x=date)) +
  geom_line(aes(y = NETRAD*coeff, color = "NETRAD")) +
  geom_line(aes(y = obs, color = "obs")) +
  geom_line(aes(y = nn_act, color = "nn_act")) +
  geom_line(aes(y = nn_pot, color = "nn_pot")) +
  labs(x = "Time", y = "mm/day", color = "Legend") +
  theme_classic()
#ylim(0, 5)
#ylim(min(out$df_cv$nn_pot), max(out$df_cv$nn_pot))
m +  scale_color_manual(values = c("black","chartreuse","brown1","cornflowerblue"))
# save plot
ggsave("ET_vs_nn_zoom.png", path = results_path, width = 5, height = 3)

##### SOIL MOISTURE BIAS #####
print("SOIL MOISTURE BIAS")
df_test <- out$df_all %>%
  mutate(bias_act = nn_act - obs,
         bias_pot = nn_pot - obs,
         soilm_bin = cut(soilm, 10)
  )

df_test %>%
  tidyr::pivot_longer(cols = c(bias_act, bias_pot), names_to = "source", values_to = "bias") %>%
  ggplot(aes(x = soilm_bin, y = bias, fill = source)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0.0), linetype = "dotted") +
  labs(title = "Bias vs. soil moisture")

# save plot
ggsave("bias_act_pot.png", path = results_path, width = 8, height = 4)

# linear regression
linmod <- lm(bias_act ~ soilm, data = df_test)
testsum <- summary(linmod)
slope_mid <- testsum$coefficients["soilm","Estimate"]
slope_se  <- testsum$coefficients["soilm","Std. Error"]
passtest_bias_vs_soilm <- ((slope_mid - slope_se) < 0 && (slope_mid + slope_se) > 0)
print(passtest_bias_vs_soilm)

df_test %>%
  ggplot(aes(x = soilm, y = bias_act)) +
  geom_point() +
  geom_smooth(method = "lm")

# save plot
ggsave("lm_bias_vs_soilmoisture.png", path = results_path, width = 5, height = 5)
dev.off()

##### NNact -- NNpot BIAS DURING MOIST DAYS (BOXPLOT) #####
print("BIAS DURING MOIST DAYS (BOXPLOT)")
df_test %>%
  tidyr::pivot_longer(cols = c(bias_act, bias_pot), names_to = "source", values_to = "bias") %>%
  dplyr::filter(moist) %>%
  ggplot(aes(y = bias, fill = source)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0), linetype = "dotted")

df_test %>%
  dplyr::filter(moist) %>%
  summarise(bias_act = mean(bias_act, na.rm = TRUE), bias_pot = mean(bias_pot, na.rm = TRUE))

# save plot
ggsave("bias_nn_moistdays_doublecheck.png", path = results_path, width = 5, height = 5)
dev.off()

##### SCATTER PLOTS OF MODELS VS OBS #####
print("SCATTER PLOTS OF MODELS VS OBS")
file = sprintf("%s/scatter_nn_act-obs_all.png", results_path)
scatterheat(out$df_cv, "nn_act", "obs", "All days", file)

file = sprintf("%s/scatter_nn_pot-obs_moist.png", results_path)
scatterheat(out$df_cv %>% dplyr::filter(moist), "nn_pot", "obs", "Moist days", file)

file = sprintf("%s/scatter_nn_pot-obs_dry.png", results_path)
scatterheat(out$df_all %>% dplyr::filter(!moist), "nn_pot", "obs", "Dry days", file)

file = sprintf("%s/scatter_nn_act-pot_all.png", results_path)
scatterheat(out$df_cv %>% dplyr::filter(moist), "nn_act", "nn_pot", "Moist Days", file)


### PRINT RESULTS ######################################################

print("PRINT RESULTS")

# libraries
library(ggplot2)
library(LSD) # for density plot

# merge CWD to results df
ddf_plot <- ddf %>%
  left_join(ddf_CWD$df, by = "date") %>%
  left_join(out$df_all, by = "date")

##### DENSITY PLOT fET vs CWD #####
print("DENSITY PLOTS")
# all data
rows=nrow(ddf_plot)
title = sprintf("%s, N = %d", sitename, rows)

file = sprintf("%s/fET_vs_CWD_density.png", results_path)
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

# only take big CWD instances
biginstances <- ddf_CWD$inst %>%
  mutate(year = lubridate::year(date_start)) %>%
  group_by(year) %>%
  dplyr::filter(deficit == max(deficit)) %>%
  pull(iinst)

ddf_plot_biginstances <- ddf_plot %>%
  dplyr::filter(iinst %in% biginstances)

rows=nrow(ddf_plot_biginstances)
title = sprintf("%s, N = %d", sitename, rows)

file = sprintf("%s/fET_vs_CWD_density_biginstances.png", results_path)
png(filename = file, width = 4, height = 4.3, units = 'in', res = 300)
plot.new()
# plot.window(xlim = c(0,300),
#             ylim = c(0,1.5))
plot.window(xlim = c(min(ddf_plot_biginstances$deficit, na.rm=TRUE),max(ddf_plot_biginstances$deficit, na.rm=TRUE)),
            ylim = c(0,1.5))
heatscatterpoints(x=ddf_plot_biginstances$deficit, y = ddf_plot_biginstances$fvar)
axis(1)
axis(2)
title(main = title, xlab = "Cumulative water deficit (mm)", ylab = "fT")
box()
dev.off()

# # color by instances
# ggplot(ddf_plot_biginstances, aes(x=deficit, y=fvar, color = iinst)) +
#   geom_point() +
#   ylim(0,1.5) +
#   ggtitle(title) +
#   theme_light()


# with fET=observations/nn_pot
rows=nrow(ddf_plot)
title = sprintf("%s, N = %d", sitename, rows)

ddf_plot <- ddf_plot %>%
  mutate(fvar_obs = obs/nn_pot)

file = sprintf("%s/fET-obs_vs_CWD_density.png", results_path)
png(filename = file, width = 4, height = 4.3, units = 'in', res = 300)
plot.new()
plot.window(xlim = c(0,300),
#plot.window(xlim = c(min(ddf_plot$deficit, na.rm=TRUE),max(ddf_plot$deficit, na.rm=TRUE)),
            ylim = c(0,1.5))
# ylim = c(min(ddf_plot$fvar, na.rm=TRUE),max(ddf_plot$fvar, na.rm=TRUE)))
# ylim = c(0,1.5))
heatscatterpoints(x=ddf_plot$deficit, y = ddf_plot$fvar_obs)
axis(1)
axis(2)
title(main = title, xlab = "CWD (mm)", ylab = "obs/nn_pot")
box()
dev.off()


##### DENSITY PLOT EF vs CWD #####
# remove noise (lit review: EF is usually between 0 and 1-1.5)
# imp: filter only at this point (otherwise would remove relevant data to train model)
ddf_plot_EF <- ddf_plot_biginstances %>% #only take big instances
  dplyr::filter(EF > 0) %>%
  dplyr::filter(EF < 1)

# normalize EF per median of value with no stress
EF_ddf_median = ddf_plot_EF %>%
  dplyr::filter(deficit < 20)
Median_EF = median(EF_ddf_median$EF, na.rm = TRUE)

ddf_plot_EF = ddf_plot_EF %>%
  mutate(EF_normalized = EF/Median_EF)

# density plot
rows=nrow(ddf_plot_EF)
title = sprintf("%s, N = %d", sitename, rows)

file = sprintf("%s/EF_vs_CWD_density.png", results_path)
png(filename = file, width = 4, height = 4.3, units = 'in', res = 300)
plot.new()
plot.window(xlim = c(0,300),
#plot.window(xlim = c(min(ddf_plot_EF$deficit, na.rm=TRUE),max(ddf_plot_EF$deficit, na.rm=TRUE)),
            ylim = c(0,1.5))
heatscatterpoints(x=ddf_plot_EF$deficit, y = ddf_plot_EF$EF_normalized)
axis(1)
axis(2)
title(main = title, xlab = "Big instances CWD (mm)", ylab = "normalized EF")
#mtext(side=0.5, line=6, at=1, adj=0, cex=0.7, col = 'black', subtitle)
box()
dev.off()

##### BILINEAR REGRESSION #####
print("BILINEAR REGRESSION")
library(segmented)
#put data in right format for calc_cwd function
ddf_CWD_fvar <- ddf_CWD$df %>%
  left_join(out$df_all %>% dplyr::select(fvar,date), by = "date")

segmented <- fvar::calc_cwd_lue0(ddf_CWD_fvar, ddf_CWD$inst, "fvar", do_plot = TRUE)
file = sprintf("%s/segmented_%s.RData", data_frames_path, sitename)
save(segmented, file = file)

segmented$gg +
  labs(title = sprintf("%s, Number of breakpoints = %d", sitename, segmented$num_splits)) +
  ylab("fET")
ggsave("fET_vs_CWD_bilinear.png", path = results_path, width = 4, height = 4)
}





