### read results from all sites and plot them together
# run on Euler directly (no need to submit job, not computational intensive)

#Load packages
library(LSD) # load basic functions of LSD and then overwrite with beni's function
devtools::load_all(".")
library(tidyverse)  
library(caret)
library(bigleaf)
library(assertive.types)
library(ggpointdensity)
library(ggrepel)
library(matlab)
library(ggpubr)

# 76 sites that passed "Successfully completed" with the NNpot = NNact(SM=1) method (important to have them to exclude older results still in output)
#site_list = c("US-Los", "CZ-wet", "BR-Sa3", "NL-Loo", "AU-How", "FR-Pue", "IT-Isp", "US-Cop", "AU-RDF", "FR-LBr", "DE-Tha", "BE-Vie", "AU-Rob", "FR-Gri", "US-ARc", "DE-SfN", "US-GLE", "RU-Fyo", "CH-Dav", "IT-Tor", "CH-Cha", "BE-Lon", "DK-Sor", "FI-Hyy", "IT-SRo", "BE-Bra", "US-ARb", "AU-Wom", "AU-Stp", "FI-Sod", "US-ARM", "ES-LgS", "AU-Ync", "DE-Seh", "AU-Tum", "AU-Emr", "US-Blo", "US-MMS", "AU-Dry", "US-Ne1", "AU-DaS", "AU-Cpr", "AU-DaP", "AR-SLu", "US-Ne3", "AU-Cum", "US-Ton", "US-Syv", "US-Ne2", "US-Whs", "US-Wi0", "US-SRM", "US-SRG", "US-WCr", "AU-Gin", "AT-Neu", "IT-CA2", "US-Var", "DE-Kli", "US-AR2", "IT-SR2", "DE-Lkb", "IT-Ro2", "DE-Obe", "IT-BCi", "IT-CA3", "IT-Ren", "US-Me2", "IT-Lav", "DE-Gri", "IT-Col", "DE-Hai", "IT-PT1", "IT-MBo", "IT-Cpz", "DE-Geb")

# 78 sites that "Successfully completed" the SM threshold method 
# sites removed manually:
# AU-Tum and US-Me2 (maxCWD not consistent, too high); "AR-SLu" (fishy ET bias profile)
# US-ARc: spike of low values in the middle of fET vs CWD, not consistent
# AU-ASM, AU-GWW: too few points in fET vs CWD biginstances
# US-Whs: noisy fET vs CWD (could be added to lowET but max CWD is too small)
 
site_list = c("AT-Neu", "AU-Cpr", "AU-Cum", "AU-DaP", "AU-DaS", "AU-Emr", "AU-Gin", "AU-How", 
              "AU-RDF", "AU-Rob", "AU-Stp", "AU-Wom", "AU-Ync", "BE-Bra", "BE-Lon", "BE-Vie", "BR-Sa3", 
              "CH-Dav", "CZ-wet", "DE-Geb", "DE-Gri", "DE-Hai", "DE-Kli", "DE-Lkb", "DE-Obe", "DE-Seh", 
              "DE-SfN", "DE-Tha", "DK-Sor", "ES-LgS", "FI-Hyy", "FI-Sod", "FR-Gri", "FR-LBr", "FR-Pue", 
              "IT-BCi", "IT-CA2", "IT-CA3", "IT-Col", "IT-Cpz", "IT-Isp", "IT-Lav", "IT-MBo", "IT-Noe", 
              "IT-PT1", "IT-Ren", "IT-Ro2", "IT-SR2", "IT-SRo", "IT-Tor", "NL-Loo", "RU-Fyo", "US-AR2", 
              "US-ARb", "US-ARM", "US-Blo", "US-Cop", "US-GLE", "US-Los", "US-MMS", "US-Ne1", "US-Ne2", 
              "US-Ne3", "US-SRG", "US-SRM", "US-Syv", "US-Ton", "US-Var", "US-WCr", "US-Whs", "US-Wi0"
              )


### FILTER SITES BASED ON MODEL PERFORMANCE ############################
file_locations_out <- list.files("/Users/fgiardina/fvar/output", # define directory madre che contiene tutte le altre
                                 glob2rx("out_*.RData"), # search for this pattern inside directory
                                 full.names = TRUE, # list all paths 
                                 recursive = TRUE # go through all subdirectories
)


# CREATE DF WITH MODEL STATS PER SITE
summary_allsites_raw <- do.call( # come una loop ma scritta in forma compatta
  "rbind", # incolla dataframes together (function che verrà applicata)
  lapply( # apply function to a list of objects 
    file_locations_out,  # list of all the segmented.Rdata
    function(file){ # file è l'elemento i della lista (percorro la lista riga per riga)
      load(file) # carico il df corrispondente a ogni riga
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
      mean_AET = mean(out$df_all %>% dplyr::filter(!moist) %>% pull(nn_act), na.rm = TRUE) # pull directly extracts column as a vector (and not a df)
      mean_PET = mean(out$df_all %>% dplyr::filter(!moist) %>% pull(nn_pot), na.rm = TRUE)
      
      # merge all together
      # R2 NNact-Obs here is different than in printed results (here is with CV, in results with df_all)
      data.frame( # creo un dataframe con cio' che mi serve di quel df 
        name_of_site = substr(basename(file), 5, 10),
        num_rows = nrow(out$df_all),
        R2_obs_act_alldays = as.numeric(R2_obs_act_alldays[1,3]), # metto as.numeric cosi non prende anche il nome della colonna
        R2_obs_act_alldays_notCV = as.numeric(R2_obs_act_alldays_notCV[1,3]),
        mean_AET = mean_AET, # mean AET < mean PET during dry days 
        mean_PET = mean_PET,
        R2_obs_pot_moistdays = as.numeric(R2_obs_pot_moistdays[1,3]),
        R2_act_pot_moistdays = as.numeric(R2_act_pot_moistdays[1,3]),
        R2_obs_pot_drydays = as.numeric(R2_obs_pot_drydays[1,3]),
        R2_obs_pot_alldays = as.numeric(R2_obs_pot_alldays[1,3]),
        rmse_obs_act_alldays = as.numeric(rmse_obs_act_alldays[1,3]), # metto as.numeric cosi non prende anche il nome della colonna
        rmse_obs_pot_moistdays = as.numeric(rmse_obs_pot_moistdays[1,3]),
        rmse_act_pot_moistdays = as.numeric(rmse_act_pot_moistdays[1,3]),
        percent_dry = length(which(out$df_all$moist == FALSE))/nrow(out$df_all)
      ) 
    }
  )
)

# filter sites based on model performance and number of points
summary_allsites <- summary_allsites_raw %>%
  dplyr::filter(num_rows > 300) %>% # only take sites with at least ~1 year of points
  dplyr::filter(name_of_site %in% site_list) %>% # take only sites that come from last run of model
  dplyr::filter(R2_obs_act_alldays_notCV > 0.5) %>% # we decide to take R2 not from CV to include 3 more sites (most importantly the Finnish one, that has lower SM measurements)
  mutate(                                     #less stringent rule for mean PET and AET (to avoid excluding sites that otherwise have good results, like US-MMS)
    mean_PET_round = round(mean_PET, 1),      # round means at the first decimal
    mean_AET_round = round(mean_AET, 1)
  ) %>% 
  dplyr::filter(mean_PET_round >= mean_AET_round) # also include if they're equal (that is the case for most wet sites)

save(summary_allsites, file = "./output/summary_allsites.RData") 

# extract final number of sites and save
vec_sites = summary_allsites$name_of_site
save(vec_sites, file = "./output/vec_sites.RData") 


### SCATTER PLOTS ALL SITES POOLED TOGETHER ############################
file_locations_out <- list.files("/Users/fgiardina/fvar/output", # define directory madre che contiene tutte le altre
                             glob2rx("out_*.RData"), # search for this pattern inside directory
                             full.names = TRUE, # list all paths 
                             recursive = TRUE # go through all subdirectories
                             )

scatter_plots_raw <- do.call( # come una loop ma scritta in forma compatta
  "rbind", # incolla dataframes together (function che verrà applicata)
  lapply( # apply function to a list of objects 
    file_locations_out,  # list of all files
    function(file){ # file è l'elemento i della lista (percorro la lista riga per riga)
      load(file) # carico il df corrispondente a ogni riga
      print(file) # print filename so that I know where it fails
      
      out$df_all <- out$df_all %>%  # df_cv doesn't change scatter plots (ma visto che non ha fvar, i density plot non funzionano)
        mutate(name_site = substr(basename(file), 5, 10))

    }
  )
)

# filter only selected sites 
scatter_plots <- scatter_plots_raw %>%
  dplyr::filter(name_site %in% vec_sites) 



####*** SCATTERS ML MODEL ***####
# Figure 1A
file = sprintf("./output/scatter_nn_act-obs_allsites_1A.png")
scatterheat(scatter_plots, "obs", "nn_act", "All days", file)

# Figure 1B
file = sprintf("./output/scatter_nn_pot-obs_moist_1B.png")
scatterheat(scatter_plots %>% dplyr::filter(moist), "obs", "nn_pot", "Moist days", file)

# now in supplementary figures
file = sprintf("./output/scatter_nn_pot-obs_dry.png")
scatterheat(scatter_plots %>% dplyr::filter(!moist), "obs", "nn_pot", "Dry days", file)

file = sprintf("./output/scatter_nn_act-pot_all.png")
scatterheat(scatter_plots %>% dplyr::filter(moist), "nn_act", "nn_pot", "Moist Days", file)


####*** SCATTERS LINEAR MODEL ***####
file_locations_df <- list.files("/Users/fgiardina/fvar/output", # define directory madre che contiene tutte le altre
                                glob2rx("ddf_??????.RData"), # search for this pattern inside directory
                                full.names = TRUE, # list all paths 
                                recursive = TRUE # go through all subdirectories
)

# extract netrad and temperature
ddf_allsites <- do.call( # come una loop ma scritta in forma compatta
  "rbind", # incolla dataframes together (function che verrà applicata)
  lapply( # apply function to a list of objects 
    file_locations_df,  # list of all the segmented.Rdata
    function(file){ # file è l'elemento i della lista (percorro la lista riga per riga)
      load(file) # carico il df corrispondente a ogni riga
      print(file) # print filename so that I know where it fails
      
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

# scatter linear model 
file = sprintf("./output/scatter_nn_pot-obs_moist_linear_1D.png")
scatterheat(scatter_linear %>% dplyr::filter(moist), "obs", "NETRAD_mass_coeff", "Moist days", file)
  

####*** SCATTERS PRIESTLEY-TAYLOR MODEL ***####
load("/Users/fgiardina/data/PET_Splash/PET_output.Rdata") # load PET from SPLASH

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

# scatter priestley-taylor
file = sprintf("./output/scatter_nn_pot-obs_moist_priestley_1C.png")
scatterheat(scatter_priestley %>% dplyr::filter(moist), "obs", "pet_splash_coeff", "Moist days", file)

####*** SCATTERS GLDAS ***#### (not used)
# load GLDAS DF for all sites
file_locations_out <- list.files("/Users/fgiardina/fvar/output_GLDAS", # define directory madre che contiene tutte le altre
                                 glob2rx("ddf_gldas_??????.RData"), # search for this pattern inside directory
                                 full.names = TRUE, # list all paths 
                                 recursive = TRUE # go through all subdirectories
)

GLDAS_allsites_raw <- do.call( # come una loop ma scritta in forma compatta
  "rbind", # incolla dataframes together (function che verrà applicata)
  lapply( # apply function to a list of objects 
    file_locations_out,  # list of all files
    function(file){ # file è l'elemento i della lista (percorro la lista riga per riga)
      load(file) # carico il df corrispondente a ogni riga
      print(file) # print filename so that I know where it fails
      
      ddf_gldas <- ddf_gldas %>% 
        mutate(name_site = substr(basename(file), 11, 16))
      
    }
  )
)

scatter_gldas <- scatter_plots %>% 
  left_join(GLDAS_allsites_raw %>% 
              mutate(PET_gldas = PET) %>% 
              dplyr::select(-PET, -ET, -P, -fvar)
            , by = c("date", "name_site")) 

# scatter gldas
file = sprintf("./output/scatter_nn_pot-obs_moist_gldas.png")
scatterheat(scatter_gldas %>% dplyr::filter(moist), "obs", "PET_gldas", "Moist days", file)


### BINNING ###########################################################
# GET EF for all sites (used to double check fET)
file_locations_df <- list.files("/Users/fgiardina/fvar/output", # define directory madre che contiene tutte le altre
                                 glob2rx("ddf_??????.RData"), # search for this pattern inside directory
                                 full.names = TRUE, # list all paths 
                                 recursive = TRUE # go through all subdirectories
)

EF_allsites <- do.call( # come una loop ma scritta in forma compatta
  "rbind", # incolla dataframes together (function che verrà applicata)
  lapply( # apply function to a list of objects 
    file_locations_df,  # list of all the segmented.Rdata
    function(file){ # file è l'elemento i della lista (percorro la lista riga per riga)
      load(file) # carico il df corrispondente a ogni riga
      print(file) # print filename so that I know where it fails
      
      ddf <- ddf %>% 
        mutate(name_site = substr(basename(file), 5, 10)) %>% 
        dplyr::select(date, EF, name_site)
      
    }
  )
)

# GET CWD FOR ALL SITES
file_locations_CWD <- list.files("/Users/fgiardina/fvar/output", # define directory madre che contiene tutte le altre
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

save(plot_allsites_raw, file = "./output/plot_allsites_raw.RData")  

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
detach(package:plyr) # remove conflict with plyr (dplyr:: in front of group_by won't be enough!)
clusters <- plot_allsites_raw %>% 
  dplyr::filter(fvar < 1.5) %>% # focus on 0-1.5 interval 
  dplyr::filter(fvar > 0) %>% 
  group_by(name_site) %>% 
  dplyr::filter(between(fvar, ### REMOVE outliers in the distribution of every site (statistically)
                        mean(fvar, na.rm=TRUE) - (2.5 * sd(fvar, na.rm=TRUE)), 
                        mean(fvar, na.rm=TRUE) + (2.5 * sd(fvar, na.rm=TRUE)))) %>% 
  dplyr::filter(deficit > 125) %>% # filter only CWD values around 150 mm
  dplyr::filter(deficit < 175) %>% 
  dplyr::group_by(name_site) %>% 
  summarise(
    median_fvar = median(fvar, na.rm = TRUE) # calculate median of fvar inside every bin
  ) %>% 
  ungroup()


# cluster along fvar median in CWD bin centered around 150 mm
set.seed(23)
cluster_data <- kmeans(clusters$median_fvar, 3, iter.max = 10, nstart = 25)
clusters$cluster = factor(cluster_data$cluster)

# change clusters name
clusters$cluster <- gsub('3', 'low fET', clusters$cluster)
clusters$cluster <- gsub('1', 'medium fET', clusters$cluster)
clusters$cluster <- gsub('2', 'high fET', clusters$cluster)
clusters$cluster <- factor(clusters$cluster, levels = c("low fET", "medium fET", "high fET"))


####*** CHARTS BINNING ***####

#### Histogram
# calculate bin width (Freedman-Diaconis rule)
bw <- (2 * IQR(clusters$median_fvar)) / (length(clusters$median_fvar)^(1/3)) # Freedman-Diaconis rule not only considers the sample size 
# but also considers the spread of the sample.

# color by cluster
mu <- plyr::ddply(clusters, "cluster", summarise, grp.mean=mean(median_fvar)) # calculate average weight of each group 
a <- ggplot(clusters, aes(x=median_fvar, color=cluster, fill=cluster)) +
  geom_histogram(alpha=0.5, position="identity", binwidth = bw) +   # position="stack"
  geom_vline(data=mu, aes(xintercept=grp.mean, color=cluster),
             linetype="dashed") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=12),
    legend.position=c(.25,.9),
    legend.title=element_blank()
  ) +
  scale_y_reverse() +
  coord_flip() +
  xlab("fET (-)") +   # median at CWD = 150 mm
  ylab("Number of sites") +
  scale_x_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) 
a

# fVAR all sites
b <- heatscatter(x=plot_allsites_fvar$deficit, y = plot_allsites_fvar$fvar, ggplot = TRUE)
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14)
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310)) +
  geom_vline(xintercept = 125, linetype="dashed", 
             color = "red") +
  geom_vline(xintercept = 175, linetype="dashed", 
             color = "red") 
b

ggarrange(a, b,
          labels = c("a", "b"),
          ncol = 2, nrow = 1
          ) 
ggsave("Fig_binning.png", path = "./", width = 8, height = 4)


####*** CHARTS fET vs CWD (DNN) ***####

# add clusters to df
plot_allsites_fvar <- plot_allsites %>% 
  left_join(clusters, by = "name_site") %>% 
  mutate(cluster = replace(cluster, is.na(cluster), "high fET"))  # manually add spare sites to 'high ET' bin
save(plot_allsites_fvar, file = "./output/plot_allsites_fvar.RData")  

# overwrite 'clusters' with the sites added manually
clusters <- plot_allsites_fvar %>% 
  dplyr::select(name_site, median_fvar, cluster) %>% 
  unique()
clusters$cluster <- factor(clusters$cluster, levels = c("low fET", "medium fET", "high fET"))

# directly load df
load("./output/00_Figures_final/dataframes/plot_allsites_fvar.RData")

### PANEL A: high fET 
# get site names inside bin
sites_a <- clusters %>% 
  dplyr::filter(cluster=="high fET") 
sites_a <- as.character(sites_a$name_site)

# filter 
plot_a <- plot_allsites_fvar %>% 
  dplyr::filter(cluster == "high fET")

# plot 
a <- heatscatter(x=plot_a$deficit, y = plot_a$fvar, ggplot = TRUE) #ylorrd
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16), # center and bold title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310))
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16), # center and bold title
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm") 
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310))
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310)) +
  plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
e



####*** CHARTS GLDAS ***####
# GLDAS DF
file_locations_out <- list.files("/Users/fgiardina/fvar/output_GLDAS", # define directory madre che contiene tutte le altre
                                 glob2rx("ddf_gldas_??????.RData"), # search for this pattern inside directory
                                 full.names = TRUE, # list all paths 
                                 recursive = TRUE # go through all subdirectories
)

GLDAS_allsites_raw <- do.call( # come una loop ma scritta in forma compatta
  "rbind", # incolla dataframes together (function che verrà applicata)
  lapply( # apply function to a list of objects 
    file_locations_out,  # list of all files
    function(file){ # file è l'elemento i della lista (percorro la lista riga per riga)
      load(file) # carico il df corrispondente a ogni riga
      print(file) # print filename so that I know where it fails
      
      ddf_gldas <- ddf_gldas %>% 
        mutate(name_site = substr(basename(file), 11, 16))
      
    }
  )
)

# filter
GLDAS_allsites <- GLDAS_allsites_raw %>% 
  dplyr::filter(PET >0) %>% # remove Inf values for fET
  dplyr::filter(ET >0) %>% 
  dplyr::filter(fvar < 1.5) %>% # focus on 0-1.5 interval 
  dplyr::filter(fvar > 0) %>% 
  group_by(name_site) %>% 
  dplyr::filter(between(fvar, ### REMOVE outliers in the distribution of every site (statistically)
                        mean(fvar, na.rm=TRUE) - (2.5 * sd(fvar, na.rm=TRUE)), 
                        mean(fvar, na.rm=TRUE) + (2.5 * sd(fvar, na.rm=TRUE)))) %>% 
  ungroup() 


### get CWD GLDAS
# GET CWD FOR ALL SITES
file_locations_CWD <- list.files("/Users/fgiardina/fvar/output_GLDAS", # define directory madre che contiene tutte le altre
                                 glob2rx("ddf_CWD_gldas_??????.RData"), # search for this pattern inside directory
                                 full.names = TRUE, # list all paths 
                                 recursive = TRUE # go through all subdirectories
)

# IT-Cpz is empty (somehow not extracted from original NetCDF data)
CWD_allsites_gldas <- do.call( # come una loop ma scritta in forma compatta
  "rbind", # incolla dataframes together (function che verrà applicata)
  lapply( # apply function to a list of objects 
    file_locations_CWD,  # list of all the segmented.Rdata
    function(file){ # file è l'elemento i della lista (percorro la lista riga per riga)
      load(file) # carico il df corrispondente a ogni riga
      print(file) # print filename so that I know where it fails
      
      if(!dim(ddf_CWD_gldas$inst)[1] == 0){ # avoid empty df (no data could be extracted)
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
  left_join(CWD_allsites_gldas, by = c("date", "name_site")) %>% 
  dplyr::filter(name_site %in% vec_sites) %>% 
  na.omit() 

# rescale GLDAS fvar to have maximum around 1 
plot_allsites_gldas <- plot_allsites_gldas %>%
  group_by(name_site) %>%
  mutate(
    fvar_scaled = fvar / median(fvar[deficit < 20]) # take the median of the lower bin of the deficit
  ) %>% 
  ungroup()

# multiply PET by scaling constant based on AET (work less effectively than scaling above)
lm_model = lm(ET ~ PET + 0, plot_allsites_gldas)
coeff = lm_model[["coefficients"]][["PET"]]

plot_allsites_gldas = plot_allsites_gldas %>% 
  mutate(PET_k = PET*coeff) %>% 
  mutate(fvar_k = ET/PET_k)

# save
save(plot_allsites_gldas, file = "./output/plot_allsites_gldas.RData") 

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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 500, 100), limits = c(0, 500))
z
ggsave("allsitesGLDAS.png", path = "./", width = 4, height = 4)

  
### PANEL B: low fET GLDAS
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310)) +
  plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310)) +
  plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
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
    title = "Medium fET"
  ) +
  theme_classic() +
  theme(
    axis.text=element_text(size = 14),
    axis.title=element_text(size = 16),
    legend.text=element_text(size=14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310)) +
  plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
f

####*** Compose final chart - deep learning and GLDAS ***####
ggarrange(a, b, c, d, e, f,
          labels = "auto",
          ncol = 2, nrow = 3,
          common.legend = TRUE, # have just one common legend
          legend="top") # and place it in the bottom

# save plot
ggsave("alldensity.png", path = "./", width = 8, height = 12)


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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300))
  
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310)) 
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310)) 
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16) # center and bold title
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 310)) 
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
  # mutate(cluster = ifelse(cluster==1,"wet",cluster)) %>% 
  # mutate(cluster = ifelse(cluster==3,"dry",cluster)) %>% 
  # mutate(cluster = ifelse(cluster==2,"superdry",cluster)) %>% 
  mutate(lon = round(lon, 2)) %>% 
  mutate(lat = round(lat, 2))

save(table1, file = "./output/table1_raw.RData") 

# adjust table formatting (raggruppare colonne e rimuovere non necessarie) 
table1$Coordinates <- paste0(table1$lon, ", ", table1$lat)  #paste0 non mette spazi di default
table1$Years <- paste(table1$year_start, "-", table1$year_end)
table1 <- table1 %>% 
  dplyr::select(-lon, -lat, -year_start, -year_end) %>% 
  dplyr::relocate(Coordinates, .after = name_site) %>% 
  dplyr::relocate(Years, .after = Coordinates)

# add MAP/MAP
sitelist_fluxnet <- read.csv("~/data/FLUXNET-2015_Tier1/fluxnet2015_MAT-MAP.csv") 

table1 <- table1 %>% 
  left_join(sitelist_fluxnet %>% 
              dplyr::select(-SITE_NAME, -FLUXNET2015, -FLUXNET.CH4, -LOCATION_LAT, -LOCATION_LONG, -LOCATION_ELEV, -IGBP) %>% 
              rename(name_site = SITE_ID)
            , 
            by = c("name_site")
            ) 

save(table1, file = "./output/table1.RData") 


