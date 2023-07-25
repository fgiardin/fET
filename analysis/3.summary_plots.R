### read results from all sites and plot them

#Load packages
library(LSD) # load basic functions of LSD and then overwrite with our functions based on LSD
devtools::load_all(".")
library(tidyverse)
library(caret)
library(bigleaf)
library(assertive.types)
library(ggpointdensity)
library(ggrepel)
library(matlab)
library(ggpubr)
source("~/fET/R/LSD.heatscatter.R")

# List of sites that "Successfully completed" script 2.run_ML_model_euler.R

# sites removed manually upfront:
# AU-Tum and US-Me2 (maxCWD not consistent, too high);
# BE-Lon and AR-SLu fishy ET bias profile, noisy fET vs CWD plot
# US-ARc: spike of low values in the middle of fET vs CWD, not consistent
# AU-ASM, AU-GWW: too few points in fET vs CWD biginstances
# US-Whs, FI-Hyy: noisy fET vs CWD (also: could be added to lowET but max CWD is too small)


site_list = c("AT-Neu", "AU-Cpr", "AU-Cum", "AU-DaP", "AU-DaS", "AU-Emr", "AU-Gin", "AU-How",
              "AU-RDF", "AU-Rob", "AU-Stp", "AU-Wom", "AU-Ync", "BE-Bra", "BE-Vie", "BR-Sa3",
              "CH-Dav", "CZ-wet", "DE-Geb", "DE-Gri", "DE-Hai", "DE-Kli", "DE-Lkb", "DE-Obe", "DE-Seh",
              "DE-SfN", "DE-Tha", "DK-Sor", "ES-LgS", "FI-Sod", "FR-Gri", "FR-LBr", "FR-Pue",
              "IT-BCi", "IT-CA2", "IT-CA3", "IT-Col", "IT-Cpz", "IT-Isp", "IT-Lav", "IT-MBo", "IT-Noe",
              "IT-PT1", "IT-Ren", "IT-Ro2", "IT-SR2", "IT-SRo", "IT-Tor", "NL-Loo", "RU-Fyo", "US-AR2",
              "US-ARb", "US-ARM", "US-Blo", "US-Cop", "US-GLE", "US-Los", "US-MMS", "US-Ne1", "US-Ne2",
              "US-Ne3", "US-SRG", "US-SRM", "US-Syv", "US-Ton", "US-Var", "US-WCr", "US-Wi0")


### FILTER SITES BASED ON MODEL PERFORMANCE ############################

# get list of files we want to merge (here: output dataframes from script 2)
file_locations_out <- list.files("data/output", # define directory where all files are
                                 glob2rx("out_*.RData"), # search for this pattern inside directory
                                 full.names = TRUE, # list all paths
                                 recursive = TRUE # go through all subdirectories
)


# CREATE DF WITH MODEL PERFORMANCE STATS PER SITE
summary_allsites_raw <- do.call( # like a 'for' loop but in a more compact form
  "rbind", # function that will be applied to dataframes calculated below (it binds dataframes together)
  lapply( # apply function to a list of objects
    file_locations_out,  # list of files found above
    function(file){ # file = ith element of the list (row by row)
      load(file) # load the file
      print(file) # print filename so that I know where it fails

      # calculate model stats
      R2_obs_act_alldays = out$df_cv %>% na.omit %>% yardstick::rsq(nn_act,obs)
      R2_obs_act_alldays_notCV = out$df_all %>% na.omit %>% yardstick::rsq(nn_act,obs)
      R2_obs_pot_moistdays = out$df_all %>% dplyr::filter(moist) %>% na.omit %>% yardstick::rsq(nn_pot,obs)
      R2_obs_pot_drydays = out$df_all %>% dplyr::filter(!moist) %>% na.omit %>% yardstick::rsq(nn_pot,obs)
      R2_obs_pot_alldays = out$df_all %>% na.omit %>% yardstick::rsq(nn_pot,obs)
      R2_act_pot_moistdays = out$df_all %>% dplyr::filter(moist) %>% na.omit %>% yardstick::rsq(nn_pot,nn_act)
      rmse_obs_act_alldays = out$df_cv %>% na.omit %>% yardstick::rmse(nn_act,obs)
      rmse_obs_pot_moistdays = out$df_all %>% dplyr::filter(moist) %>% na.omit %>% yardstick::rmse(nn_pot,obs)
      rmse_act_pot_moistdays = out$df_all %>% dplyr::filter(moist) %>% na.omit %>% yardstick::rmse(nn_pot,nn_act)
      # mean AET < mean PET during dry days
      mean_AET = mean(out$df_all %>% dplyr::filter(!moist) %>% pull(nn_act), na.rm = TRUE)
      mean_PET = mean(out$df_all %>% dplyr::filter(!moist) %>% pull(nn_pot), na.rm = TRUE)

      # Create dataframe with stats
      data.frame(
        name_of_site = substr(basename(file), 5, 10),
        num_rows = nrow(out$df_all),
        R2_obs_act_alldays = as.numeric(R2_obs_act_alldays[1,3]), # as.numeric removes the name of the column (output of yardstick)
        R2_obs_act_alldays_notCV = as.numeric(R2_obs_act_alldays_notCV[1,3]),
        mean_AET = mean_AET,
        mean_PET = mean_PET,
        R2_obs_pot_moistdays = as.numeric(R2_obs_pot_moistdays[1,3]),
        R2_act_pot_moistdays = as.numeric(R2_act_pot_moistdays[1,3]),
        R2_obs_pot_drydays = as.numeric(R2_obs_pot_drydays[1,3]),
        R2_obs_pot_alldays = as.numeric(R2_obs_pot_alldays[1,3]),
        rmse_obs_act_alldays = as.numeric(rmse_obs_act_alldays[1,3]),
        rmse_obs_pot_moistdays = as.numeric(rmse_obs_pot_moistdays[1,3]),
        rmse_act_pot_moistdays = as.numeric(rmse_act_pot_moistdays[1,3]),
        percent_dry = length(which(out$df_all$moist == FALSE))/nrow(out$df_all)
      )
    }
  )
)

# filter sites based on model performance and number of points
summary_allsites <- summary_allsites_raw %>%
  dplyr::filter(num_rows > 300) %>% # take sites with at least ~1 year of points
  dplyr::filter(name_of_site %in% site_list) %>% # take sites that come from last run of model
  dplyr::filter(R2_obs_act_alldays_notCV > 0.5) %>% # we decide to take R2 not from CV to include 3 more sites (most importantly the Finnish one, that has lower SM measurements)
  mutate(                                     #less stringent rule for mean PET and AET (to avoid excluding sites that otherwise have good results, like US-MMS)
    mean_PET_round = round(mean_PET, 1),      # round means at the first decimal
    mean_AET_round = round(mean_AET, 1)
  ) %>%
  dplyr::filter(mean_PET_round >= mean_AET_round) # include also if they're equal (that is the case for most wet sites)

save(summary_allsites, file = "./summary_allsites.RData")

# extract final number of sites and save
vec_sites = unique(summary_allsites$name_of_site)
save(vec_sites, file = "./vec_sites.RData")


### SCATTER PLOTS ALL SITES POOLED TOGETHER ############################

# same list as above
file_locations_out <- list.files("data/output",
                             glob2rx("out_*.RData"),
                             full.names = TRUE,
                             recursive = TRUE
                             )

# this time we just need the output of the model
scatter_plots_raw <- do.call(
  "rbind",
  lapply(
    file_locations_out,
    function(file){
      load(file)
      print(file)

      out$df_all <- out$df_all %>%
        mutate(name_site = substr(basename(file), 5, 10))

    }
  )
)

# filter only selected sites
scatter_plots <- scatter_plots_raw %>%
  dplyr::filter(name_site %in% vec_sites)



####*** SCATTERS ML MODEL ***####
# supplementary figures
file = sprintf("./scatter_nn_act-pot_all_S1A.png")
scatterheat(scatter_plots %>% dplyr::filter(moist), "nn_act", "nn_pot", "Moist Days", file)

file = sprintf("./scatter_nn_pot-obs_dry_S1B.png")
scatterheat(scatter_plots %>% dplyr::filter(!moist), "obs", "nn_pot", "Dry days", file)


####*** SCATTERS LINEAR MODEL ***####
# load dataframes used as model input (cleaned data)
file_locations_df <- list.files("data/output",
                                glob2rx("ddf_??????.RData"),
                                full.names = TRUE,
                                recursive = TRUE
                                )

# extract netrad and temperature
ddf_allsites <- do.call(
  "rbind",
  lapply(
    file_locations_df,
    function(file){
      load(file)
      print(file)

      ddf <- ddf %>%
        mutate(name_site = substr(basename(file), 5, 10)) %>%
        mutate(NETRAD_mass = LE.to.ET(NETRAD, TA_F)*24*60*60) %>%
        dplyr::select(date, NETRAD_mass, name_site)
    }
  )
)

scatter_linear = scatter_plots %>%
  left_join(ddf_allsites, by = c("date", "name_site"))

# get coefficient to scale netrad
lm_model = lm(obs ~ NETRAD_mass + 0, scatter_linear)
coeff = lm_model[["coefficients"]][["NETRAD_mass"]]

scatter_linear = scatter_linear %>%
  mutate(NETRAD_mass_coeff = NETRAD_mass*coeff)



####*** SCATTERS PRIESTLEY-TAYLOR MODEL ***####
df_output <- readRDS("data/PET_output.rds") # load PET from SPLASH

# initialize empty dataframe
scatter_priestley_raw <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("date", "pet_splash", "name_site"))

for (site_ID in vec_sites){
  print(site_ID)
  # filter row with our site
  PET_SPLASH = df_output %>%
  dplyr::filter(sitename==site_ID)

  if(dim(PET_SPLASH)[1] == 0){} # se il df non esiste, non fare niente
  else{
  # extract the dataframe
  df_PET_SPLASH = PET_SPLASH[[2]][[1]] %>%
    mutate(date = lubridate::ymd(date)) %>%
    dplyr::select(date, pet) %>%
    rename(pet_splash = pet) %>%
    mutate(name_site = site_ID)

  scatter_priestley_raw <- rbind(scatter_priestley_raw, df_PET_SPLASH)
  }
}

scatter_priestley = scatter_plots %>%
  left_join(scatter_priestley_raw, by = c("date", "name_site"))

# get coefficient to scale netrad
lm_model = lm(obs ~ pet_splash + 0, scatter_priestley)
coeff = lm_model[["coefficients"]][["pet_splash"]]

scatter_priestley = scatter_priestley %>%
  mutate(pet_splash_coeff = pet_splash*coeff)


####*** compose final figure scatters ***####
devtools::load_all(".")
# Figure 1A
file = sprintf("./scatter_nn_act-obs_allsites_1A.png")
a <- scatterheat(scatter_plots, "obs", "nn_act", "All days", file)

# Figure 1B
file = sprintf("./scatter_nn_pot-obs_moist_1B.png")
b <- scatterheat(scatter_plots %>% dplyr::filter(moist), "obs", "nn_pot", "Moist days", file)

# scatter priestley-taylor
file = sprintf("./scatter_nn_pot-obs_moist_priestley_1C.png")
c <- scatterheat(scatter_priestley %>% dplyr::filter(moist), "obs", "pet_splash_coeff", "Moist days", file)

# scatter linear model
file = sprintf("./scatter_nn_pot-obs_moist_linear_1D.png")
d <- scatterheat(scatter_linear %>% dplyr::filter(moist), "obs", "NETRAD_mass_coeff", "Moist days", file)

all <- ggarrange(a, b, c, d,
          labels = "auto",
          ncol = 2, nrow = 2) # and place it in the bottom
# save plot
ggsave("allscatters.png", path = "./", width = 8, height = 9, dpi = 600)

# save scatter plot data for additional figures
scatter_plots_all = scatter_plots %>%
  left_join(scatter_priestley %>% dplyr::select(date, name_site, pet_splash, pet_splash_coeff), by = c("date", "name_site")) %>%
  left_join(scatter_linear %>% dplyr::select(date, name_site, NETRAD_mass, NETRAD_mass_coeff), by = c("date", "name_site"))

saveRDS(scatter_plots_all, "./scatter_plots_all.rds", compress = "xz")

### BINNING ###########################################################
# GET EF for all sites (used to double check fET)
file_locations_df <- list.files("data/output",
                                 glob2rx("ddf_??????.RData"),
                                 full.names = TRUE,
                                 recursive = TRUE
)

EF_allsites <- do.call(
  "rbind",
  lapply(
    file_locations_df,
    function(file){
      load(file)
      print(file)

      ddf <- ddf %>%
        mutate(name_site = substr(basename(file), 5, 10)) %>%
        dplyr::select(date, EF, name_site)

    }
  )
)

# GET CWD FOR ALL SITES
file_locations_CWD <- list.files("data/output", # define directory madre che contiene tutte le altre
                                glob2rx("ddf_CWD_??????.RData"), # search for this pattern inside directory
                                full.names = TRUE, # list all paths
                                recursive = TRUE # go through all subdirectories
)

CWD_allsites <- do.call( # come una loop ma scritta in forma compatta
  "rbind", # incolla dataframes together (function che verrà applicata)
  lapply( # apply function to a list of objects
    file_locations_CWD,  # list of all the segmented.Rdata
    function(file){ # file è l'elemento i della lista (percorro la lista riga per riga)
      load(file) # carico il df corrispondente a ogni riga
      print(file) # print filename so that I know where it fails

      # big instances
      biginstances <- ddf_CWD$inst %>%
        mutate(year = lubridate::year(date_start)) %>%
        group_by(year) %>%
        dplyr::filter(deficit == max(deficit)) %>%
        pull(iinst)

      ddf_CWD$df <- ddf_CWD$df %>%
        mutate(name_site = substr(basename(file), 9, 14)) %>%
        dplyr::filter(iinst %in% biginstances) # only take biginstances

    }
  )
)

# merge CWD and fET to results (out)
plot_allsites_raw <- CWD_allsites %>%
  left_join(scatter_plots, by = c("date", "name_site")) %>%
  left_join(EF_allsites, by = c("date", "name_site")) %>%
  dplyr::filter(name_site %in% vec_sites) %>%
  na.omit()

save(plot_allsites_raw, file = "./plot_allsites_raw.RData")

# filter outliers
plot_allsites <- plot_allsites_raw %>%
  dplyr::filter(fvar < 1.5) %>%
  dplyr::filter(fvar > 0) %>%
  group_by(name_site) %>%
  filter(between(fvar,
                 mean(fvar, na.rm=TRUE) - (2.5 * sd(fvar, na.rm=TRUE)), # remove outliers by site
                 mean(fvar, na.rm=TRUE) + (2.5 * sd(fvar, na.rm=TRUE))
                 )
         ) %>%
  ungroup()

####*** Binning by median fET around CWD = 150 mm ***####
#detach(package:plyr) # remove conflict with plyr (dplyr:: in front of group_by won't be enough!)
clusters <- plot_allsites_raw %>%
  dplyr::filter(fvar < 1.5) %>% # focus on 0-1.5 interval
  dplyr::filter(fvar > 0) %>%
  group_by(name_site) %>%
  dplyr::filter(between(fvar, ### REMOVE outliers in the distribution of every site
                        mean(fvar, na.rm=TRUE) - (2.5 * sd(fvar, na.rm=TRUE)),
                        mean(fvar, na.rm=TRUE) + (2.5 * sd(fvar, na.rm=TRUE)))) %>%
  dplyr::filter(deficit > 125) %>% # filter only CWD values around 150 mm
  dplyr::filter(deficit < 175) %>%
  dplyr::group_by(name_site) %>%
  summarise(
    median_fvar = median(fvar, na.rm = TRUE) # calculate median of fvar inside every bin
  ) %>%
  ungroup()


# group sites along fvar median in CWD bin centered around 150 mm
set.seed(23)
cluster_data <- kmeans(clusters$median_fvar, 3, iter.max = 10, nstart = 25)
clusters$cluster = factor(cluster_data$cluster)

# change clusters name
clusters$cluster <- gsub('2', 'low fET', clusters$cluster)
clusters$cluster <- gsub('1', 'medium fET', clusters$cluster)
clusters$cluster <- gsub('3', 'high fET', clusters$cluster)
clusters$cluster <- factor(clusters$cluster, levels = c("low fET", "medium fET", "high fET"))

# add clusters to df
plot_allsites_fvar <- plot_allsites %>%
  left_join(clusters, by = "name_site") %>%
  mutate(cluster = replace(cluster, is.na(cluster), "high fET"))  # manually add spare sites to 'high ET' bin
# those are: US-Syv, US-Los, IT-Ren, IT-Lav, DE-Tha, DE-Obe, DE-Lkb, DE-Kli
save(plot_allsites_fvar, file = "./plot_allsites_fvar.RData")

# overwrite 'clusters' with the sites added manually
clusters <- plot_allsites_fvar %>%
  dplyr::select(name_site, median_fvar, cluster) %>%
  unique()
clusters$cluster <- factor(clusters$cluster, levels = c("low fET", "medium fET", "high fET"))


####*** CHARTS BINNING ***####

#### Histogram
# calculate bin width (Freedman-Diaconis rule)
bw <- (2 * IQR(clusters$median_fvar, na.rm = TRUE)) / (length(clusters$median_fvar)^(1/3)) # Freedman-Diaconis rule not only considers the sample size
                                                                            # but also the spread of the sample.

# change name of groups for plot
clusters_plot <- clusters %>%
  mutate(cluster = str_replace(cluster, "fET", "fET sites")) %>%
  droplevels() # drop old levels
clusters_plot$cluster <- factor(clusters_plot$cluster, levels = c("low fET sites", "medium fET sites", "high fET sites"))

# calculate average weight of each group (average fET inside each group)
mu <- plyr::ddply(clusters_plot, "cluster", summarise, grp.mean=mean(median_fvar, na.rm = TRUE))

a <- ggplot(clusters_plot,
            aes(x=median_fvar,
                color=cluster,
                fill=cluster)
            ) +
  geom_histogram(alpha=0.5, position="identity", binwidth = bw) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=12),
    legend.position=c(.75,.9),
    legend.title=element_blank()
  ) +
  #scale_y_reverse() + # flip around y axis
  coord_flip() +
  xlab(expression(paste(fET[paste(CWD, "=", 150)], " (-)"))) + # nested expression() with two "paste()" inside, one for the entire subscript and the other for the units
  ylab("Number of sites") +
  scale_x_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=cluster),
             linetype="dashed")  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16), breaks = seq(0, 15, 5))
plot(a)

# fVAR all sites
b <- heatscatter(x=plot_allsites_fvar$deficit, y = plot_allsites_fvar$fvar, ggplot = TRUE)
b <- b +
  labs(
    y = "fET (-)",
    x = "Cumulative water deficit (mm)"
    ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14)
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0)) +
  geom_vline(xintercept = 125, linetype="dashed",
             color = "red") +
  geom_vline(xintercept = 175, linetype="dashed",
             color = "red")
b

ggarrange(b, a,
          labels = c("a", "b"),
          ncol = 2, nrow = 1
          )
ggsave("Fig_binning.png", path = "./", width = 8, height = 4)


####*** CHARTS fET vs CWD (DNN) ***####

### PANEL A: high fET
# get site names inside bin
sites_a <- clusters %>%
  dplyr::filter(cluster=="high fET")
sites_a <- as.character(sites_a$name_site)

# filter
plot_a <- plot_allsites_fvar %>%
  dplyr::filter(name_site %in% sites_a)

# plot
a <- heatscatter(x=plot_a$deficit, y = plot_a$fvar, ggplot = TRUE)
a <- a +
  labs(
    y = "fET (-)",
    x = "Cumulative water deficit (mm)",
    title = "High fET"
    ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 16), # center title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) + # 'expand' # removes space between axis and plotted data (!!!)
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
a

### PANEL C: medium fET
# get site names inside bin
sites_c <- clusters %>%
  dplyr::filter(cluster=="medium fET")
sites_c <- as.character(sites_c$name_site)

# filter
plot_c <- plot_allsites_fvar %>%
  dplyr::filter(name_site %in% sites_c)

# plot
c <- heatscatter(x=plot_c$deficit, y = plot_c$fvar, ggplot = TRUE)
c <- c +
  labs(
    y = "fET (-)",
    x = "Cumulative water deficit (mm)",
    title = "Medium fET"
    ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 16), # center and bold title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
c

### PANEL E: low fET
# get site names inside bin
sites_e <- clusters %>%
  dplyr::filter(cluster=="low fET")
sites_e <- as.character(sites_e$name_site)

# filter
plot_e <- plot_allsites_fvar %>%
  dplyr::filter(name_site %in% sites_e)

# plot
e <- heatscatter(x=plot_e$deficit, y = plot_e$fvar, ggplot = TRUE)
e <- e +
  labs(
    y = "fET (-)",
    x = "Cumulative water deficit (mm)",
    title = "Low fET"
    ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 16),    # center and bold title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
print(e)


####*** CHARTS GLDAS ***####
# GLDAS DF
file_locations_out <- list.files("data/output_GLDAS",
                                 glob2rx("ddf_gldas_??????.RData"),
                                 full.names = TRUE,
                                 recursive = TRUE
)

GLDAS_allsites_raw <- do.call(
  "rbind",
  lapply(
    file_locations_out,
    function(file){
      load(file)
      print(file)

      ddf_gldas <- ddf_gldas %>%
        mutate(name_site = substr(basename(file), 11, 16))

    }
  )
)


# filter
GLDAS_allsites <- GLDAS_allsites_raw %>%
  dplyr::filter(PET > 0) %>% # remove Inf values for fET
  dplyr::filter(ET > 0) %>%
  mutate(fvar = ET/PET) %>% # recalculate fvar to avoid infinite values when dividing by zero
  dplyr::filter(fvar < 1.5) %>% # focus on 0-1.5 interval
  dplyr::filter(fvar > 0) %>%
  group_by(name_site) %>%
  dplyr::filter(between(fvar, ### REMOVE outliers in the distribution of every site
                        mean(fvar, na.rm=TRUE) - (2.5 * sd(fvar, na.rm=TRUE)),
                        mean(fvar, na.rm=TRUE) + (2.5 * sd(fvar, na.rm=TRUE)))) %>%
  ungroup()

# plot_allsites_gldas <- plot_allsites_fvar %>%
#   left_join(plot_allsites_gldas, by = c("date", "name_site")) %>%
#   drop_na()


### get CWD GLDAS
# GET CWD FOR ALL SITES
file_locations_CWD <- list.files("data/output_GLDAS",
                                 glob2rx("ddf_CWD_gldas_??????.RData"),
                                 full.names = TRUE,
                                 recursive = TRUE
)


CWD_allsites_gldas <- do.call(
  "rbind",
  lapply(
    file_locations_CWD,
    function(file){
      load(file)
      print(file)

      if(!dim(ddf_CWD_gldas$inst)[1] == 0){ # avoid empty df (when no data could be extracted)
        # big instances
        biginstances <- ddf_CWD_gldas$inst %>%
          mutate(year = lubridate::year(date_start)) %>%
          group_by(year) %>%
          dplyr::filter(deficit == max(deficit)) %>%
          pull(iinst)

        ddf_CWD_gldas$df <- ddf_CWD_gldas$df %>%
          mutate(name_site = substr(basename(file), 15, 20)) %>%
          dplyr::filter(iinst %in% biginstances) # only take biginstances
      }
    }
  )
)

# merge GLDAS and CWD for all sites
plot_allsites_gldas <- GLDAS_allsites %>%
  left_join(CWD_allsites_gldas %>% dplyr::select(-iinst, -dday, -water_balance), by = c("date", "name_site")) %>%
  dplyr::filter(name_site %in% vec_sites) %>%
  drop_na()

# rescale GLDAS fvar to have maximum around 1
plot_allsites_gldas <- plot_allsites_gldas %>%
  group_by(name_site) %>%
  mutate(
    fvar_scaled = fvar / median(fvar[deficit < 20]) # take the median of the lower bin of the deficit
  ) %>%
  ungroup()

# save
save(plot_allsites_gldas, file = "./plot_allsites_gldas.RData")

### Plot all sites GLDAS
z <- heatscatter(x=plot_allsites_gldas$deficit, y = plot_allsites_gldas$fvar_scaled, ggplot = TRUE)
z <- z +
  labs(
    y = expression(paste("fET"["GLDAS"]*" (-)")),
    x = "Cumulative water deficit (mm)",
    title = "All sites GLDAS"
    ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 500, 100), limits = c(0, 500), expand = c(0, 0))
z
ggsave("allsitesGLDAS.png", path = "./", width = 4, height = 4)


### PANEL B: high fET GLDAS
# filter
plot_b <- plot_allsites_gldas %>%
  dplyr::filter(name_site %in% sites_a) # panels a and b have same sites

# plot
b <- heatscatter(x=plot_b$deficit, y = plot_b$fvar_scaled, ggplot = TRUE)
b <- b +
  labs(
    y = expression(paste("fET"["GLDAS"]*" (-)")),
    x = "Cumulative water deficit (mm)",
    title = "High fET"
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 16), # center and bold title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
b

### PANEL D: medium fET GLDAS
# filter
plot_d <- plot_allsites_gldas %>%
  dplyr::filter(name_site %in% sites_c)  # panels c and d have same sites

# plot
d <- heatscatter(x=plot_d$deficit, y = plot_d$fvar_scaled, ggplot = TRUE)
d <- d +
  labs(
    y = expression(paste("fET"["GLDAS"]*" (-)")),
    x = "Cumulative water deficit (mm)",
    title = "Medium fET"
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 16), # center and bold title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
d

### PANEL F: low fET GLDAS
# filter
plot_f <- plot_allsites_gldas %>%
  dplyr::filter(name_site %in% sites_e)  # panels e and f have same sites

# plot
f <- heatscatter(x=plot_f$deficit, y = plot_f$fvar_scaled, ggplot = TRUE)
f <- f +
  labs(
    y = expression(paste("fET"["GLDAS"]*" (-)")),
    x = "Cumulative water deficit (mm)",
    title = "Low fET"
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 16), # center and bold title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
f

####*** Compose final chart - deep learning and GLDAS ***####
ggarrange(a, b, c, d, e, f,
          labels = "auto",
          ncol = 2, nrow = 3,
          common.legend = TRUE, # have just one common legend
          legend="top") # and place it in the bottom

# save plot
ggsave("alldensity.png", path = "./", width = 8, height = 12)

# Distributions of CWD  ----------------------------------------------------
# plot distributions of CWD for GLDAS vs FLUXNET to use in supplementary material

# FLUXNET
w <- plot_e %>%
  ggplot(aes(x = deficit)) +
  geom_density(color ="grey30", fill = "#B2D0AA", position="identity", alpha=0.5) +
  # geom_histogram(alpha=0.5, position="identity", color ="grey30", fill = "#B2D0AA") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  xlim(0,300) +
  scale_y_continuous(breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.01), limits = c(0, 0.01)) +
  # scale_y_continuous(breaks = c(0, 250, 500, 750, 1000), limits = c(0, 1100)) +
  xlab("Cumulative water deficit (mm)") +
  ylab("Density") +
  ggtitle("FLUXNET2015")
w

# GLDAS
z <- plot_f %>%
  ggplot(aes(x = deficit)) +
  geom_density(color ="grey30", fill = "#B2D0AA", position="identity", alpha=0.5) +
  # geom_histogram(alpha=0.5, position="identity", color ="grey30", fill = "#B2D0AA") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  xlim(0,300) +
  scale_y_continuous(breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.01), limits = c(0, 0.01)) +
  # scale_y_continuous(breaks = c(0, 250, 500, 750, 1000), limits = c(0, 1100)) +
  xlab("Cumulative water deficit (mm)") +
  ylab("Density") +
  ggtitle("GLDAS-NOAH")
z

# plot together
library(patchwork)

# plot together using patchwork
(w | z) +
  plot_annotation(tag_levels ="a") &
  theme(plot.tag = element_text(face="bold"))

# save plot
ggsave("CWD_distributions.png", path = "./", width = 10, height = 5)


####*** CHARTS EF ***####
# remove outliers and scale
plot_allsites_EF <- plot_allsites_raw %>%
  na.omit() %>%
  group_by(name_site) %>%
  filter(between(EF,
                 mean(EF, na.rm=TRUE) - (2.5 * sd(EF, na.rm=TRUE)), # remove outliers by site
                 mean(EF, na.rm=TRUE) + (2.5 * sd(EF, na.rm=TRUE))
  )
  ) %>%
  dplyr::filter(EF > 0) %>%
  dplyr::filter(EF < 1) %>%
  mutate(
    EF_scaled = EF / median(EF[deficit < 20]) # take the median of the lower bin of the deficit
  ) %>%
  ungroup()

### Panel A: EF all sites
a <- heatscatter(x=plot_allsites_EF$deficit, y = plot_allsites_EF$EF_scaled, ggplot = TRUE)
a <- a +
  labs(
    y = "EF (-)",
    x = "Cumulative water deficit (mm)",
    title = "All sites EF"
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.title = element_text(hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 310), expand = c(0, 0))
a

### Panel B: high EF
# filter
plot_b <- plot_allsites_EF %>%
  dplyr::filter(name_site %in% sites_a)

# plot
b <- heatscatter(x=plot_b$deficit, y = plot_b$EF_scaled, ggplot = TRUE)
b <- b +
  labs(
    y = "EF (-)",
    x = "Cumulative water deficit (mm)",
    title = "High fET"
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.title = element_text(hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
b

### Panel c: medium EF
# filter
plot_c <- plot_allsites_EF %>%
  dplyr::filter(name_site %in% sites_c)

# plot
c <- heatscatter(x=plot_c$deficit, y = plot_c$EF_scaled, ggplot = TRUE)
c <- c +
  labs(
    y = "EF (-)",
    x = "Cumulative water deficit (mm)",
    title = "Medium fET"
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.title = element_text(hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
c

### Panel d: low EF
# filter
plot_d <- plot_allsites_EF %>%
  dplyr::filter(name_site %in% sites_e)

# plot
d <- heatscatter(x=plot_d$deficit, y = plot_d$EF_scaled, ggplot = TRUE)
d <- d +
  labs(
    y = "EF (-)",
    x = "Cumulative water deficit (mm)",
    title = "Low fET"
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.title = element_text(hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310), expand = c(0, 0))
d


### combine EF
ggarrange(a, b, c, d,
          labels = c("a", "b", "c", "d"),
          ncol = 2, nrow = 2,
          common.legend = TRUE, # have just one common legend
          legend="top") # and place it in the bottom

# save plot
ggsave("EF_combined.png", path = "./", width = 8, height = 8)



### TABLE 1 ############################
siteinfo_fluxnet <- read.csv("~/nn_fluxnet2015/siteinfo_fluxnet2015_sofun.csv")

table1 <- siteinfo_fluxnet %>%
  dplyr::select(mysitename, lon, lat, year_start, year_end, classid) %>%
  dplyr::filter(mysitename %in% vec_sites) %>%
  rename(name_site = mysitename) %>%
  left_join(clusters %>% dplyr::select(name_site, cluster), by = c("name_site")) %>%
  mutate(lon = round(lon, 2)) %>%
  mutate(lat = round(lat, 2))

# save table in this format (used by data-raw/extract_HWSD.R)
save(table1, file = "./table1_raw.RData")

# adjust table formatting for table 1 (group columns and remove unnecesessary ones)
table1$Coordinates <- paste0(table1$lon, ", ", table1$lat)  # paste0 does not put spaces
table1$Years <- paste(table1$year_start, "-", table1$year_end)
table1 <- table1 %>%
  dplyr::select(-lon, -lat, -year_start, -year_end) %>%
  dplyr::relocate(Coordinates, .after = name_site) %>%
  dplyr::relocate(Years, .after = Coordinates)

# add MAP/MAP from fluxnet (several Australian sites are missing --> we used WorldClim)
sitelist_fluxnet <- read.csv("~/data/FLUXNET-2015_Tier1/fluxnet2015_MAT-MAP.csv")

table1 <- table1 %>%
  left_join(sitelist_fluxnet %>%
              dplyr::select(-SITE_NAME, -FLUXNET2015, -FLUXNET.CH4, -LOCATION_LAT, -LOCATION_LONG, -LOCATION_ELEV, -IGBP) %>%
              rename(name_site = SITE_ID)
            ,
            by = c("name_site")
            )
save(table1, file = "./table1.RData")


# Calculate percentage of missing data based on number of predictors ----------------
vec_sites <- c("AU-Cpr", "AU-Cum", "AU-DaP", "AU-DaS", "AU-Gin", "AU-How",
               "AU-RDF", "AU-Stp", "AU-Wom", "BE-Vie", "BR-Sa3", "CH-Dav",
               "DE-Geb", "DE-Kli", "DE-Lkb", "DE-Obe", "DE-Seh", "DE-Tha",
               "DK-Sor", "FR-LBr", "FR-Pue", "IT-BCi", "IT-CA3", "IT-Col",
               "IT-Cpz", "IT-Lav", "IT-Noe", "IT-Ren", "IT-Ro2", "IT-SR2",
               "IT-SRo", "IT-Tor", "NL-Loo", "RU-Fyo", "US-AR2", "US-ARb",
               "US-ARM", "US-Blo", "US-Los", "US-MMS", "US-Ne1", "US-Ne2",
               "US-Ne3", "US-SRG", "US-SRM", "US-Syv", "US-Ton", "US-Var",
               "US-WCr")

file_locations <- list.files("data/output",
                                glob2rx("hhdf_??????.RData"),
                                full.names = TRUE,
                                recursive = TRUE
)

# extract netrad and temperature
hhdf_allsites <- do.call(
  "rbind",
  lapply(
    file_locations,
    function(file){
      load(file)
      print(file)

      hhdf <- hhdf %>%
        mutate(name_site = substr(basename(file), 6, 11)) %>%
        dplyr::select(
          one_of(
            "date", # omit EVI as it has it's there in both set of predictors
            "name_site",
            "NETRAD",
            "VPD_F",
            "TA_F",
            "WS_F",
            "TS_F_MDS_1",
            "USTAR"
          )
        )
      }
    )
  )


# define two dataframes with different set of predictors and remove NAs in both
hhdf_4 <- hhdf_allsites %>%
  dplyr::filter(name_site %in% vec_sites) %>%
  dplyr::select("name_site",
                "NETRAD",
                "VPD_F",
                "TA_F") %>%
  drop_na()

hhdf_7 <- hhdf_allsites %>%
  dplyr::select("name_site",
                "NETRAD",
                "VPD_F",
                "TA_F",
                "WS_F",
                "TS_F_MDS_1",
                "USTAR") %>%
  drop_na()

# find percentage of missing data compared to the original data (focusing only on our final list of sites)
perc_4 <- 1 - nrow(hhdf_4 %>% dplyr::filter(name_site %in% vec_sites))/
  nrow(hhdf_allsites %>% dplyr::filter(name_site %in% vec_sites))

perc_7 <- 1 - nrow(hhdf_7 %>% dplyr::filter(name_site %in% vec_sites))/
  nrow(hhdf_allsites %>% dplyr::filter(name_site %in% vec_sites))







