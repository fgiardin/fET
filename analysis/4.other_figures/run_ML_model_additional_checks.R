### run model for every site and plot metrics and results

### script built from 2.run_ML_model_euler.R, simplified to run for some sites only
### results are in SI

#!/usr/bin/env Rscript

# ## evaluate arguments (they are then available as args[1], args[2], ...
# args = commandArgs(trailingOnly=TRUE)
#
# # define sitename
# sitename = args[1] # you can run locally and set sitename equal to a fluxnet site name (e.g. AU-Wom)
# print(sitename)

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

library(doMC) # to run in parallel
registerDoMC(cores=200) # specify number of cores to run code in parallel // 200 for Euler, 5 when local

modelledSM = 0 # use modelled SM or not (it will use observational SM from FLUXNET)
vec_sites <- c("US-Ton", "US-Var", "AU-Stp", "DE-Tha", "IT-CA3", "US-MMS")

for(i in vec_sites) {
  print(i)
  # select PFT to print
  sitename = i

# create directory for results
dir_name = sprintf("data/output/%s", sitename) # path to directory of site (gia creata in pre_process)
data_frames_path = sprintf("%s/data_frames", dir_name) # path to dir of dataframes (gia creata)
results_path = sprintf("%s/results", dir_name)

# create results path
dir.create(dir_name)
dir.create(results_path)
dir.create(data_frames_path)

# load dataframes
# CWD + DDF
CWD_name = sprintf("%s/data_frames/ddf_CWD_%s.RData", dir_name, sitename)
ddf_name = sprintf("%s/data_frames/ddf_%s.RData", dir_name, sitename)
load(CWD_name)
load(ddf_name)


### DEFINE SETTINGS, FIND THRESHOLD AND RUN MODEL ######################

# define settings
settings <- list(
  target         = "ET",
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


# # USE MODELLED Soil Moisture IF SWC IS NOT CONSISTENT
# # load .csv with soil moisture data classified according to their usability (see Methods)
# soil_usability <- read.csv("~/data/fLUE/soilm_data_usability_fluxnet2015.csv")
#
# soil_usability_filtered <- soil_usability %>%
#   dplyr::filter(mysitename == sitename)
#
# if (soil_usability_filtered$code == 1) {
#   settings$varnams_soilm = "SWC_F_MDS_1"
# } else {
#   settings$varnams_soilm = "wcont_s11"
# }

# force modelled SM
if (modelledSM) {
  settings$varnams_soilm = "wcont_s11"
} else {
  settings$varnams_soilm = "SWC_F_MDS_1"
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


# ###*** HP TUNING ***###
# print("LAUNCH HP TUNING")
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

# load HP tuning runs
load(sprintf("data/output/%s/data_frames/runs_%s.RData", sitename, sitename))

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
if (modelledSM) {
  file = sprintf("%s/out_modelledSM_%s.RData", data_frames_path, sitename)
  save(out, file = file)
} else {
  file = sprintf("%s/out_%s.RData", data_frames_path, sitename)
  save(out, file = file)
}



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


##### SCATTER PLOTS OF MODELS VS OBS #####
print("SCATTER PLOTS OF MODELS VS OBS")

if (modelledSM) {

  file = sprintf("%s/scatter_nn_act-obs_all_modelledSM.png", results_path)
  scatterheat(out$df_cv, "nn_act", "obs", "All days", file)

  file = sprintf("%s/scatter_nn_pot-obs_moist_modelledSM.png", results_path)
  scatterheat(out$df_cv %>% dplyr::filter(moist), "nn_pot", "obs", "Moist days", file)

  file = sprintf("%s/scatter_nn_pot-obs_dry_modelledSM.png", results_path)
  scatterheat(out$df_all %>% dplyr::filter(!moist), "nn_pot", "obs", "Dry days", file)

  file = sprintf("%s/scatter_nn_act-pot_all_modelledSM.png", results_path)
  scatterheat(out$df_cv %>% dplyr::filter(moist), "nn_act", "nn_pot", "Moist Days", file)

} else {

  file = sprintf("%s/scatter_nn_act-obs_all.png", results_path)
  scatterheat(out$df_cv, "nn_act", "obs", "All days", file)

  file = sprintf("%s/scatter_nn_pot-obs_moist.png", results_path)
  scatterheat(out$df_cv %>% dplyr::filter(moist), "nn_pot", "obs", "Moist days", file)

  file = sprintf("%s/scatter_nn_pot-obs_dry.png", results_path)
  scatterheat(out$df_all %>% dplyr::filter(!moist), "nn_pot", "obs", "Dry days", file)

  file = sprintf("%s/scatter_nn_act-pot_all.png", results_path)
  scatterheat(out$df_cv %>% dplyr::filter(moist), "nn_act", "nn_pot", "Moist Days", file)

}

### PRINT RESULTS ######################################################

print("PRINT RESULTS")

# libraries
library(ggplot2)
library(LSD) # for density plot
devtools::load_all(".")

# merge CWD to results df
ddf_plot <- ddf %>%
  left_join(ddf_CWD$df, by = "date") %>%
  left_join(out$df_all, by = "date")

# only take big CWD instances
biginstances <- ddf_CWD$inst %>%
  mutate(year = lubridate::year(date_start)) %>%
  group_by(year) %>%
  dplyr::filter(deficit == max(deficit)) %>%
  pull(iinst)

ddf_plot_biginstances <- ddf_plot %>%
  dplyr::filter(iinst %in% biginstances)

a <- heatscatter(x=ddf_plot_biginstances$deficit, y = ddf_plot_biginstances$fvar, ggplot = TRUE)
a <- a +
  labs(
    y = "fET (-)",
    x = "Cumulative water deficit (mm)",
    title = sitename
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 16, face="bold"), # center title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) + # 'expand' # removes space between axis and plotted data (!!!)
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))

if (modelledSM) {
  ggsave("fET_vs_CWD_density_biginstances_modelledSM.png", path = "results_path", width = 4, height = 4)
} else {
  ggsave("fET_vs_CWD_density_biginstances.png", path = "results_path", width = 4, height = 4)
}


}



