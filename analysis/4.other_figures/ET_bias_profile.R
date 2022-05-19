### print ET bias profile
library(grid)
library(tidyverse)
library(ggpubr)

### panel A ####
sitename = "BE-Vie" # manually insert sitename
load("data/output/BE-Vie/data_frames/out_BE-Vie.RData") # load output of ML model
load("data/output/BE-Vie/data_frames/ddf_BE-Vie.RData") # load input of ML model

# merge
df_test1 <- out$df_all %>% dplyr::select(date, fvar) %>% # extract fET
  left_join(ddf %>% dplyr::select(date, wcont_s11), by = "date") %>% # append SM
  mutate(SM_maxnorm = wcont_s11/(max(wcont_s11, na.rm = TRUE))) %>% # calculate SM divided by maxSM of that site
  mutate(SM_maxnorm_bin = cut(SM_maxnorm, 10, dig.lab = 1)) %>%  # create bins (1 digit only)
  na.omit() %>%
  dplyr::filter(fvar < 1.5) %>%  # clean outliers in fET
  dplyr::filter(fvar > 0) %>%
  filter(between(fvar,
                 mean(fvar, na.rm=TRUE) - (2.5 * sd(fvar, na.rm=TRUE)), # remove outliers by site
                 mean(fvar, na.rm=TRUE) + (2.5 * sd(fvar, na.rm=TRUE))
                )
          )

grob_a <- grobTree(textGrob(sitename, x=0.01,  y=0.05, hjust=0,
                          gp=gpar(col="black", fontsize=14, fontface="bold")))

a <- df_test1 %>%
  ggplot(aes(x = SM_maxnorm_bin, y = fvar)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(fill="#D6D6E9") +
  theme_classic() +
  geom_hline(aes(yintercept = 1), linetype = "dotted") +
  labs(x = "Soil Moisture (%)", y = "fET (-)") +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.4)) +
  annotation_custom(grob_a) + # add llabel with name of site
  theme(legend.position="bottom", legend.direction="horizontal", legend.justification = "left") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14)
        )
a


### panel B ####
sitename = "IT-CA3" # manually insert sitename
load("data/output/IT-CA3_SWC_F_MDS_4/data_frames/out_IT-CA3.RData") # load output
load("data/output/IT-CA3_SWC_F_MDS_4/data_frames/ddf_IT-CA3.RData") # load input

# merge
df_test2 <- out$df_all %>% dplyr::select(date, fvar) %>% # extract fET
  left_join(ddf %>% dplyr::select(date, SWC_F_MDS_4), by = "date") %>% # append SM
  mutate(SM_maxnorm = SWC_F_MDS_4/(max(SWC_F_MDS_4, na.rm = TRUE))) %>% # calculate SM divided by maxSM of that site
  mutate(SM_maxnorm_bin = cut(SM_maxnorm, 10, dig.lab = 1)) %>% # create bins
  na.omit() %>%
  dplyr::filter(fvar < 1.5) %>%  # clean outliers in fET
  dplyr::filter(fvar > 0) %>%
  filter(between(fvar,
                 mean(fvar, na.rm=TRUE) - (2.5 * sd(fvar, na.rm=TRUE)), # remove outliers by site
                 mean(fvar, na.rm=TRUE) + (2.5 * sd(fvar, na.rm=TRUE))
  )
  )

grob_b <- grobTree(textGrob(sitename, x=0.01,  y=0.05, hjust=0,
                          gp=gpar(col="black", fontsize=14, fontface="bold")))

b <- df_test2 %>%
  ggplot(aes(x = SM_maxnorm_bin, y = fvar)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(fill="#D6D6E9") +
  theme_classic() +
  geom_hline(aes(yintercept = 1), linetype = "dotted") +
  labs(x = "Soil Moisture (%)", y = "fET (-)") +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5)) +
  annotation_custom(grob_b) + # add name of site label
  theme(legend.position="bottom", legend.direction="horizontal", legend.justification = "left") +
  #scale_x_discrete(labels=c("(0,0.1]", "(0.1,0.2]", "(0.2,0.3]", "(0.3,0.4]", "(0.4,0.5]", "(0.5,0.6]", "(0.6,0.7]", "(0.7,0.8]", "(0.8,0.9]", "(0.9,1]")) + # round x axis ticks
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14)  #,face="bold"
        )
b


### panel C ####
file_locations_out <- list.files("data/output",
                                 glob2rx("out_*.RData"),
                                 full.names = TRUE,
                                 recursive = TRUE
)

# create df with data from all sites
ddf_out <- do.call(
  "rbind",
  lapply(
    file_locations_out,
    function(file){
      load(file)
      print(file)

      out$df_all <- out$df_all %>%
        mutate(name_site = substr(basename(file), 5, 10)) %>%
        mutate(bias_act = nn_act - obs,
               bias_pot = nn_pot - obs,
               bias_nn = nn_pot - nn_act,
               bias_nn_norm = bias_nn/as.numeric(quantile(nn_act, 0.75, na.rm = TRUE)), # as.numeric only takes the results and not the "75%" 'title'
               soilm_bin = cut(soilm, 10)
        )

    }
  )
)

# get raw soil moisture (not normalized between 0 and 1 as in output df)
file_locations_df <- list.files("data/output",
                                glob2rx("ddf_??????.RData"),
                                full.names = TRUE,
                                recursive = TRUE
)

ddf_allsites <- do.call(
  "rbind",
  lapply(
    file_locations_df,
    function(file){
      load(file)
      print(file)

      if(dim(ddf)[1] == 0){} # check if the dataframe is empty to avoid errors
      else{
      ddf <- ddf %>%
        mutate(name_site = substr(basename(file), 5, 10)) %>%
        dplyr::select(date, name_site, wcont_s11) %>%
        na.omit() %>%
        mutate(SM_maxnorm = wcont_s11/(max(wcont_s11, na.rm = TRUE))) %>% # calculate SM divided by maxSM of that site
        mutate(SM_maxnorm_bin = cut(SM_maxnorm, 10))  # create bins
      }

    }
  )
)

# merge two dataframes
ddf_merge <- ddf_out %>% dplyr::select(date, fvar, name_site) %>% # extract fET
  left_join(ddf_allsites %>% dplyr::select(date, wcont_s11, SM_maxnorm_bin, name_site), by = c("date", "name_site")) %>% # append SM
  dplyr::filter(fvar < 1.5) %>%  # clean outliers in fvar
  dplyr::filter(fvar > 0) %>%
  filter(between(fvar,
                 mean(fvar, na.rm=TRUE) - (2.5 * sd(fvar, na.rm=TRUE)), # remove outliers by site
                 mean(fvar, na.rm=TRUE) + (2.5 * sd(fvar, na.rm=TRUE))
  )
  )



# find sites where we used modelled SM
soil_usability <- read.csv("data/soilm_data_usability_fluxnet2015.csv") # should work both on Euler and local
soil_usability_filtered <- soil_usability %>%
  dplyr::filter(code != 1) # only retain sites where we need to use modelled SM

# load current list of final sites
load("manuscript/Figures/dataframes/vec_sites.RData")

# intersection of all sites that have modelled SM with final list of sites
mysites_withmodelledSM <- intersect(soil_usability_filtered$mysitename,vec_sites)

c <- ddf_merge %>%
  dplyr::filter(name_site %in% mysites_withmodelledSM) %>%  # filter sites
  ggplot(aes(x = fct_reorder(name_site, fvar, .desc = FALSE), y = fvar)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(fill="#D6D6E9") +
  theme_classic() +
  geom_hline(aes(yintercept = 1), linetype = "dotted") +
  labs(y = "fET (-)") +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text=element_text(size=10),
    axis.title=element_text(size=14),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) # arrange x axis labels to be readable
    ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.2), limits = c(0, 1.5))
plot(c)



# combine graphs
ggarrange(a, b, c,
          labels = "auto",
          ncol = 1, nrow = 3
)


# save plot
ggsave("ET_bias_profile_nn_norm.png", path = "./", width = 8, height = 10)

