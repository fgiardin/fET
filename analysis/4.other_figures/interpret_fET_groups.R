# Code to plot current figure 5 to interpret cluster / see how other variables (aridity index, soil texture, etc.) 
# are related to clusters

#Load packages
devtools::load_all(".")
library(tidyverse)  
library(hwsdr) # Koen's package to download HWSD database // #to see available data from: hwsd_meta_data
library(ggpubr)
library(ingestr)
library(grid)

# load table with soil texture data
load("~/fvar/output/00_Figures_4/dataframes/table1_soil.RData")
table1_soil <- table1 %>% 
  dplyr::select(-cluster, -fifth_percentile)
  

# load updated table
load("~/fvar/output/00_Figures_final/table1/table1.RData")
table1_soil <- table1_soil %>% 
  right_join(table1 %>% dplyr::select(name_site, cluster), by = "name_site") %>% # replace with current cluster names
  dplyr::select(-MAT, -MAP)                                                    # right_join cosi mi tiene i 52 siti finali 
 
# extract median fvar
load("~/fvar/output/00_Figures_final/dataframes/plot_allsites_fvar.RData")
median_fvar <- plot_allsites_fvar %>% 
  dplyr::select(name_site, median_fvar) %>% 
  unique()

# append median fvar and rename table (so it's consistent with the rest of the code)
table1 <- table1_soil %>% 
  left_join(median_fvar, by = "name_site")

# reverse order of factors (so that high fET is first, consistently with fET vs CWD figures) 
table1$cluster <- factor(table1$cluster, levels=rev(levels(table1$cluster)))


# #### extract HWSDR data (it takes some minutes to run) #######################
# # initialize empty dataframe
# soil_variables <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("AWC_CLASS","T_CLAY", "S_CLAY", "T_SAND", "S_SAND", "T_SILT", "S_SILT"))
# # it takes time to run! 
# for (i in 1:nrow(table1)){
#   print(i)
#   
#   variables_site <- ws_subset(  # LAT, LON e non LON, LAT!!!
#     site = "HWSD",
#     location = c(table1$lat[i], table1$lon[i]),
#     param = c("AWC_CLASS","T_CLAY", "S_CLAY", "T_SAND", "S_SAND", "T_SILT", "S_SILT")
#   )
#   
#   variables_site <- variables_site %>% 
#     dplyr::select(-site, -latitude, -longitude) %>%  # menzionare in methods di arrotondamento lon e lat 
#     pivot_wider(names_from = parameter, values_from = value)
#   
#   soil_variables <- rbind(soil_variables, variables_site)
#   }
# 
# # add to table
# table1 <- cbind(table1, soil_variables)
# save(table1, file = "./output/table1_soil.RData") 

### FIGURE 6: DISCUSSION ####################################
### PANEL A: Soil texture ####################################

# calculate median of soil fraction per bin
table1_soil <- table1 %>%
  group_by(cluster) %>%
  summarise(
    topclay_mean = round(mean(T_CLAY, na.rm = TRUE), 2),  # use mean so that we still have sum of fractions = 1
    topsilt_mean = round(mean(T_SILT, na.rm = TRUE), 2),
    topsand_mean = round(mean(T_SAND, na.rm = TRUE), 2),
    subclay_mean = round(mean(S_CLAY, na.rm = TRUE), 2),  # use mean so that we still have sum of fractions = 1
    subsilt_mean = round(mean(S_SILT, na.rm = TRUE), 2),
    subsand_mean = round(mean(S_SAND, na.rm = TRUE), 2),
  ) %>%
  ungroup() %>%
  pivot_longer(!cluster, names_to = "soil_type", values_to = "perc_weight") %>% 
  mutate(soil_type = factor(soil_type,
                            levels = c("topclay_mean", "topsilt_mean", "topsand_mean", "subclay_mean", "subsilt_mean", "subsand_mean"),
                            labels = c("Topsoil clay fraction", "Topsoil silt fraction", "Topsoil sand fraction", "Subsoil clay fraction", "Subsoil silt fraction", "Subsoil sand fraction")
                            ))

save(table1_soil, file = "./output/table1_soil.RData") 

# plot
a <- ggplot(
  table1_soil %>% dplyr::filter(grepl('Top', soil_type)), # select only topsoil
  aes(x = cluster, y = perc_weight , fill = soil_type)) +
  geom_bar(stat = "identity") + # stat = "identity" needs y
  theme_classic() +
  scale_fill_manual(
    values = c(
      "Topsoil clay fraction" = "#E3CBB1",
      "Topsoil silt fraction" = "#97AEB6",
      "Topsoil sand fraction" = "#997772"),
    labels = c("Clay", 
               "Silt", 
               "Sand")
  ) +
  theme(
    legend.title=element_blank(), # remove legend title (useless)
    axis.text.x=element_text(size = 12, color = "black"),
    axis.title.x=element_blank(),
    axis.text.y=element_text(size = 12),
    axis.title=element_text(size = 16),
    legend.text=element_text(size = 12),
    axis.line=element_blank(),  # remove axis line (otherwise overlap with box)
    panel.border = element_rect(  # add box around figure
      color = "black",
      fill = NA,
      size = 1),
    #plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        ) + 
  ylab("% weight") 
plot(a)

# subsoil
ab <- ggplot(
  table1_soil %>% dplyr::filter(grepl('Sub', soil_type)), # select only topsoil
  aes(x = cluster, y = perc_weight , fill = soil_type)) +
  geom_bar(stat = "identity") + # stat = "identity" needs y
  theme_classic() +
  theme(axis.line=element_blank()) +
  scale_fill_manual(
    values = c(
      "Subsoil clay fraction" = "#E3CBB1",
      "Subsoil silt fraction" = "#97AEB6",
      "Subsoil sand fraction" = "#997772")
  ) +
  theme(legend.title=element_blank()) + # remove legend title (useless)
  theme(panel.border = element_rect(
    color = "black",
    fill = NA,
    size = 1)
  ) +
  ylab("% weight") +
  xlab("Cluster")
plot(ab)
ggsave("soil_texture_subsoil.png", path = "./", width = 9, height = 8)

### PANEL B: IGBP Class ####################################
# adjust table 
table1 <- table1 %>% 
  mutate(classid = factor(classid, # order IGBP classes (consistent with Fig. 1 of fluxnet2015 main paper)
                          levels = c("EBF", "DBF", "MF", "ENF",
                                     "WSA", "SAV", "OSH", "CSH",
                                     "GRA", "CRO", "WET"
                                     ) 
  )) 

# IGBP class
b <- ggplot(table1, aes(x = cluster, fill = classid)) +
  geom_bar()+
  theme_classic() +
  theme(axis.line=element_blank()) +
  scale_fill_manual(values = c("EBF" = "#BEF268", "DBF" = "#A6CC5C", "MF" = "#218D22", "ENF" = "#006501",
                               "WSA" = "#FE3130", "SAV" = "#8C1A1A", "CSH" = "#ff8300", "OSH" = "#cc4502",
                               "GRA" = "#C3FFC2", "CRO" = "#f3f725", "WET" = "#01BFFE")) +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text.y=element_text(size = 12),
    axis.text.x=element_text(size = 12, color = "black"),
    axis.title=element_text(size = 16),
    legend.text=element_text(size = 12),
    legend.title=element_blank(),
    panel.border = element_rect(  # box around figure
      color = "black",
      fill = NA,   
      size = 1),
    #plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  ylab("Number of sites") +
  xlab("Cluster") +
  scale_y_continuous(breaks = 1:22)
plot(b)


### PANEL C: aridity index ####################################
load("~/nn_fluxnet2015/data/ai_fluxnet2015.Rdata") 
df_ai <- df_ai %>% 
  rename(name_site = mysitename)

load("~/nn_fluxnet2015/data/soilparams_fluxnet2015.Rdata") 
df_awc <- df_soil %>% 
  rename(name_site = mysitename)

table1 <- table1 %>% 
  left_join(df_ai %>% dplyr::select(name_site, ai), by = "name_site") %>% 
  left_join(df_awc %>% dplyr::select(name_site, awc), by = "name_site")

# aridity index (from np2018)
c <- ggplot(table1, aes(x = cluster, y = ai, fill = cluster)) +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  theme_classic() +
  ylab("Aridity index (-)") +
  theme(
        axis.title.x=element_blank(), # remove text of x axis
        axis.text.y=element_text(size = 14),
        axis.text.x=element_text(size = 14, color = "black"),
        axis.title=element_text(size = 16),
        legend.text=element_blank(),
        legend.position="none",
        panel.border = element_rect(  # box around figure
          color = "black",
          fill = NA,   
          size = 1),
        axis.line=element_blank(),  # remove axis line (otherwise overlap with box)
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        ) +
  #scale_fill_brewer(palette="YlOrRd") # , direction = -1 //// breaks = table1$cluster,
  scale_fill_manual(values = c("#e9c46a", "#f4a261", "#e76f51")) +   # c("#0a9396", "#e9d8a6", "#ae2012")
  scale_y_continuous(breaks = seq(0, 2, 0.25))
plot(c) 

### PANEL D: global topographic index (GTI) ###################
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis

df_sites <- table1

# load raster (not on my HD - over 10 GB. Just load df below)

# rasta <- raster::raster("~/data/gti_marthews/ga2.nc")
# extract gti values at location
# df_gti <- raster::extract(rasta, sp::SpatialPoints(dplyr::select(df_sites, lon, lat) %>% distinct()), sp = TRUE) %>%
#   as_tibble() %>% 
#   rename(gti = GDAL.Band.Number.1) %>% 
#   right_join(df_sites, by = c("lon", "lat")) %>% 
#   relocate(name_site, lon, lat, cluster, gti) # change order
# save(df_gti, file = "./output/df_gti.RData") 

# load df with gti already extracted at site locations
load("./output/00_Figures_final/dataframes/df_gti.RData") 

d <- ggplot(df_gti, aes(x= cluster, y = gti, fill = cluster)) +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  theme_classic() +
  ylab("Global Topographic Index (-)") +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text.y=element_text(size = 14),
    axis.text.x=element_text(size = 14, color = "black"),
    axis.title=element_text(size = 16),
    legend.text=element_blank(),
    legend.position="none",
    panel.border = element_rect(  # box around figure
      color = "black",
      fill = NA,   
      size = 1),
    axis.line=element_blank(),  # remove axis line (otherwise overlap with box)
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  scale_fill_manual(values = c("#e9c46a", "#f4a261", "#e76f51")) +
  scale_y_continuous(breaks = seq(0, 10, 2))
plot(d)

### PANEL E-F: MAP and MAT ####################################
## used worldclim data for MAP and MAT since most Australian sites don't have MAP/MAT in
## the fluxnet dataframe

# load vector of final sites
load("./output/00_Figures_final/dataframes/vec_sites.RData")

settings_worldclim <- list(varnam = c("bio"))

df_worldclim <- ingest(
  siteinfo_fluxnet2015 %>% dplyr::filter(sitename %in% vec_sites), # take only 58 sites of analysis
  source    = "worldclim",
  settings  = settings_worldclim,
  dir       = "~/data/worldclim"
)

df_worldclim_reformat <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("name_site", "MAT_worldclim", "MAP_worldclim"))
#df_worldclim_reformat$name_site = vec_sites

for (site_ID in vec_sites){
  print(site_ID)
  # filter row with our site
  BIO = df_worldclim %>% 
    dplyr::filter(sitename==site_ID) 
  
  if(dim(BIO)[1] == 0){} # se il df non esiste, non fare niente
  else{
    # extract the dataframe 
    df_BIO = BIO[[2]][[1]] %>% 
      mutate(name_site = site_ID) %>% 
      rename(MAT_worldclim = bio_1) %>% 
      rename(MAP_worldclim = bio_12)
    
    df_worldclim_reformat <- rbind(df_worldclim_reformat, df_BIO)
  }
}

# merge to table 1
table1 <- table1 %>% 
  left_join(df_worldclim_reformat, by = c("name_site")) 



# MAP
e <- ggplot(table1, aes(x= cluster, y = MAP_worldclim, fill = cluster)) +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  theme_classic() +
  ylab("Mean Annual Precipitation (mm)") +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text.y=element_text(size = 14),
    axis.text.x=element_text(size = 14, color = "black"),
    axis.title=element_text(size = 16),
    legend.text=element_blank(),
    legend.position="none",
    panel.border = element_rect(  # box around figure
      color = "black",
      fill = NA,   
      size = 1),
    axis.line=element_blank(),  # remove axis line (otherwise overlap with box)
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  scale_fill_manual(values = c("#e9c46a", "#f4a261", "#e76f51")) +
  scale_y_continuous(breaks = seq(0, 2000, 250))
e

# MAT
f <- ggplot(table1, aes(x = cluster, y = MAT_worldclim, fill = cluster)) +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  theme_classic() +
  ylab("Mean Annual Temperature (°C)") +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text.y=element_text(size = 14),
    axis.text.x=element_text(size = 14, color = "black"),
    axis.title=element_text(size = 16),
    legend.text=element_blank(),
    legend.position="none",
    panel.border = element_rect(  # box around figure
      color = "black",
      fill = NA,   
      size = 1),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line=element_blank(),  # remove axis line (otherwise overlap with box)
  ) +
  scale_fill_manual(values = c("#e9c46a", "#f4a261", "#e76f51")) +
  scale_y_continuous(breaks = seq(0, 30, 5))
f


### PRINT FIGURE 6 ###########
# combine graphs 
ggarrange(a, b, c, d, e, f,
          labels = "auto", 
          ncol = 2, nrow = 3
          # common.legend = TRUE, # have just one common legend
          # legend="bottom" # consider mettere legenda sotto a tutto con nomi IGBP spelled out
) 
# save plot
ggsave("Figure_6.png", path = "./", width = 9, height = 12)

### FIGURE 7: minimum fET vs aridity index, size is soil type #################

# t critical value from performing t-test on the slope using alpha = 0.05
# if p-value smaller than 0.05, the data (x) is successfully explaining the variance in y 
# we can reject the null hypothesis that the slope is equal to zero
# la p-value che calcola il modello lm è sulla slope, non su R2 (non penso abbia senso parlare di p-value per R2)


### CLAY
# split in equal-sized groups based on soil fraction
clay_groups <- quantile(table1$T_CLAY, c(.33, .67), na.rm = TRUE) 

# regression
model_1 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_CLAY < clay_groups[1])) # forcing intercept = 0, otherwise regression lines won't be comparable
model_2 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_CLAY > clay_groups[1]) %>% dplyr::filter(T_CLAY < clay_groups[2]))
model_3 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_CLAY > clay_groups[2]))
#subtitle = sprintf("Blue: T_CLAY < %.2f, Green: %.2f < T_CLAY < %.2f, Red: T_CLAY > %.2f", clay_groups[1], clay_groups[1], clay_groups[2], clay_groups[2])

title_a = sprintf("Slope = %.2f", summary(model_1)$coefficients[,1])
title_b = sprintf("Slope = %.2f", summary(model_2)$coefficients[,1])
title_c = sprintf("Slope = %.2f", summary(model_3)$coefficients[,1] )

# p-values (has to be lower than 0.05)
summary(model_1)$coefficients[,4] 
summary(model_2)$coefficients[,4]
summary(model_3)$coefficients[,4]

grob_a <- grobTree(textGrob(title_a, x=0.02,  y=0.95, hjust=0,
                            gp=gpar(col="#0041a3", fontsize=14, fontface="bold")))
grob_b <- grobTree(textGrob(title_b, x=0.02,  y=0.89, hjust=0,
                            gp=gpar(col="#009246", fontsize=14, fontface="bold")))
grob_c <- grobTree(textGrob(title_c, x=0.02,  y=0.83, hjust=0,
                            gp=gpar(col="#CE2B37", fontsize=14, fontface="bold")))

# plot
v <- ggplot(table1) +
  geom_point(aes(x = ai, y = min_fvar, size = T_CLAY)) +
  theme_classic() +
  xlab("Aridity index") +
  ylab("Median fET") +
  xlim(0, max(table1$ai, na.rm = TRUE)) + 
  geom_abline(slope = model_1[["coefficients"]][["ai"]], intercept = 0, color = "#0041a3") +  #blue
  geom_abline(slope = model_2[["coefficients"]][["ai"]], intercept = 0, color = "#009246") +  #green
  geom_abline(slope = model_3[["coefficients"]][["ai"]], intercept = 0, color = "#CE2B37") +  #red
  labs(size = "Clay (%)") +
  theme(
    axis.text=element_text(size = 10),
    axis.title=element_text(size = 14),
    legend.text=element_text(size=10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title=element_text(size = 12),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
  ) +
  annotation_custom(grob_a) +  # add slope on figure
  annotation_custom(grob_b) +
  annotation_custom(grob_c)
plot(v)


## SILT
# split in equal-sized groups based on soil fraction
silt_groups <- quantile(table1$T_SILT, c(.33, .67), na.rm = TRUE) 

# regression
model_1 = lm(min_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SILT < silt_groups[1])) # forcing intercept = 0, otherwise regression lines won't be comparable
model_2 = lm(min_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SILT > silt_groups[1]) %>% dplyr::filter(T_SILT < silt_groups[2]))
model_3 = lm(min_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SILT > silt_groups[2]))
#subtitle = sprintf("Blue: T_SILT < %.2f, Green: %.2f < T_SILT < %.2f, Red: T_SILT > %.2f", silt_groups[1], silt_groups[1], silt_groups[2], silt_groups[2])

title_a = sprintf("Slope = %.2f", summary(model_1)$coefficients[,1])
title_b = sprintf("Slope = %.2f", summary(model_2)$coefficients[,1])
title_c = sprintf("Slope = %.2f", summary(model_3)$coefficients[,1] )

# p-values (has to be lower than 0.05)
summary(model_1)$coefficients[,4] 
summary(model_2)$coefficients[,4]
summary(model_3)$coefficients[,4]

grob_a <- grobTree(textGrob(title_a, x=0.02,  y=0.95, hjust=0,
                            gp=gpar(col="#0041a3", fontsize=14, fontface="bold")))
grob_b <- grobTree(textGrob(title_b, x=0.02,  y=0.89, hjust=0,
                            gp=gpar(col="#009246", fontsize=14, fontface="bold")))
grob_c <- grobTree(textGrob(title_c, x=0.02,  y=0.83, hjust=0,
                            gp=gpar(col="#CE2B37", fontsize=14, fontface="bold")))

# plot
w <- ggplot(table1) +
  geom_point(aes(x = ai, y = min_fvar, size = T_SILT)) +
  theme_classic() +
  xlab("Aridity index") +
  ylab("Median fET") +
  xlim(0, max(table1$ai, na.rm = TRUE)) + 
  geom_abline(slope = model_1[["coefficients"]][["ai"]], intercept = 0, color = "#0041a3") + #blue
  geom_abline(slope = model_2[["coefficients"]][["ai"]], intercept = 0, color = "#009246") + #green
  geom_abline(slope = model_3[["coefficients"]][["ai"]], intercept = 0, color = "#CE2B37") +  #red
  labs(size = "Silt (%)") +
  theme(
    axis.text=element_text(size = 10),
    axis.title=element_text(size = 14),
    legend.text=element_text(size=10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title=element_text(size = 12),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
  ) +
  annotation_custom(grob_a) +  # add slope on figure
  annotation_custom(grob_b) +
  annotation_custom(grob_c)
plot(w)

### SAND
# split in equal-sized groups based on soil fraction
sand_groups <- quantile(table1$T_SAND, c(.33, .67), na.rm = TRUE) 

model_1 = lm(min_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SAND < sand_groups[1])) # forcing intercept = 0, otherwise regression lines won't be comparable
model_2 = lm(min_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SAND > sand_groups[1]) %>% dplyr::filter(T_SAND < sand_groups[2]))
model_3 = lm(min_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SAND > sand_groups[2]))
#subtitle = sprintf("Blue: T_SAND < %.2f, Green: %.2f < T_SAND < %.2f, Red: T_SAND > %.2f", sand_groups[1], sand_groups[1], sand_groups[2], sand_groups[2])

title_a = sprintf("Slope = %.2f", summary(model_1)$coefficients[,1])
title_b = sprintf("Slope = %.2f", summary(model_2)$coefficients[,1])
title_c = sprintf("Slope = %.2f", summary(model_3)$coefficients[,1] )

# p-values (has to be lower than 0.05)
summary(model_1)$coefficients[,4] 
summary(model_2)$coefficients[,4]
summary(model_3)$coefficients[,4]

grob_a <- grobTree(textGrob(title_a, x=0.02,  y=0.95, hjust=0,
                            gp=gpar(col="#0041a3", fontsize=14, fontface="bold", fontfamily = "Arial")))  #blue
grob_b <- grobTree(textGrob(title_b, x=0.02,  y=0.89, hjust=0,
                            gp=gpar(col="#009246", fontsize=14, fontface="bold")))  #green
grob_c <- grobTree(textGrob(title_c, x=0.02,  y=0.83, hjust=0,
                            gp=gpar(col="#CE2B37", fontsize=14, fontface="bold")))  #red


# plot
z <- ggplot(table1) +
  geom_point(aes(x = ai, y = min_fvar, size = T_SAND)) +
  theme_classic() +
  xlab("Aridity index") +
  ylab("Median fET") +
  xlim(0, max(table1$ai, na.rm = TRUE)) + 
  geom_abline(slope = model_1[["coefficients"]][["ai"]], intercept = 0, color = "#0041a3") +
  geom_abline(slope = model_2[["coefficients"]][["ai"]], intercept = 0, color = "#009246") +
  geom_abline(slope = model_3[["coefficients"]][["ai"]], intercept = 0, color = "#CE2B37") +
  labs(size = "Sand (%)") +
  theme(
    axis.text=element_text(size = 10),
    axis.title=element_text(size = 14),
    legend.text=element_text(size=10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title=element_text(size = 12),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
  ) +
  annotation_custom(grob_a) +  # add slope on figure
  annotation_custom(grob_b) +
  annotation_custom(grob_c)
#scale_size(range = c(1,10))
plot(z)

ggarrange(v, z, w, 
          labels = c("a", "b", "c"),
          ncol = 2, nrow = 2
) 
# save plot
ggsave("fvar_vs_aridityindex_regressions.png", path = "./", width = 10, height = 8)


### EXTRA ####################################
# adjust df
df_S0 <- readRDS(file = "~/data/rootzone_beni/df_S0.RDS", refhook = NULL)
df_S0 <- df_S0 %>% rename(name_site = sitename)
table1 <- table1 %>% 
  left_join(df_S0 %>% dplyr::select(name_site, S0, whc, koeppen_code), by = "name_site")

# S0
m <- ggplot(table1) +
  geom_boxplot(aes(y = S0, fill = cluster)) +
  facet_grid(~cluster) +
  theme_classic() +
  ylab("Root-zone water storage capacity (mm)") +
  theme(axis.title.x=element_blank(), # remove x axis
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_brewer(palette="RdYlBu", direction=-1)
plot(m)
ggsave("root-zone_clusters.png", path = "./", width = 5, height = 4)

# whc
n <- ggplot(table1) +
  geom_boxplot(aes(y = whc, fill = cluster)) +
  facet_grid(~cluster) +
  theme_classic() +
  ylab("Water holding capacity") +
  theme(axis.ticks.x=element_blank()) +
  scale_x_discrete(labels= c("wet", "dry", "superdry")) +
  scale_fill_brewer(palette="RdYlBu", direction=-1)
plot(n)

# koeppen climate
table1 <- table1 %>% 
  mutate(cluster = factor(cluster,
                          levels = c("wet", "dry", "superdry") # change order for facets
  )) %>% 
  mutate(koeppen_code = factor(koeppen_code, # order IGBP classes (consistent with Fig. 1 of fluxnet2015 main paper)
                          levels = c("Am", "Aw" , 
                                     "Bwk", "BSh", "BSk",  
                                     "Csa", "Csb", 
                                     "Cfa", "Cfb", 
                                     "Dfa", "Dfb", "Dfc", 
                                     "ET"
                          ) 
  )) 

# levels = c("Am" = "Tropical rainforest", "Aw" = "Tropical monsoon", 
#            "Bwk" = "Cold desert", "BSh" = "Hot semi-arid", "BSk" = "Cold semi-arid",  
#            "Csa" = "Hot-summer Mediterranean", "Csb" = "Warm-summer Mediterranean", 
#            "Cfa" = "Humid subtropical", "Cfb" = "Temperate oceanic", 
#            "Dfa" = "Hot-summer humid continental", "Dfb" = "Warm-summer humid continental", "Dfc" = "Subarctic", 
#            "ET" = "Tundra")
# scale_fill_manual(values = c("Tropical rainforest" = "#0077FF", "Tropical monsoon" = "#46A9FA", 
#                              "Cold desert" = "#FE9695", "Hot semi-arid" = "#F5A301", "Cold semi-arid" = "#FFDB63",
#                              "Hot-summer Mediterranean" = "#FFFF00", "Warm-summer Mediterranean" = "#C6C700",
#                              "Humid subtropical" = "#C6FF4E", "Temperate oceanic" = "#66FF33", 
#                              "Hot-summer humid continental" = "#00FFFF", "Warm-summer humid continental" = "#38C7FF", "Subarctic" = "#007E7D",
#                              "Tundra" = "#B2B2B2"
# )) 

# Koeppen climate class
o <- ggplot(table1, aes(x = cluster, fill = koeppen_code)) +
  geom_bar()+
  theme_classic() +
  theme(axis.line=element_blank()) +
  
  scale_fill_manual(values = c("Am" = "#0077FF", "Aw" = "#46A9FA", 
                               "Bwk" = "#FE9695", "BSh" = "#F5A301", "BSk" = "#FFDB63",
                               "Csa" = "#FFFF00", "Csb" = "#C6C700",
                               "Cfa" = "#C6FF4E", "Cfb" = "#66FF33", 
                               "Dfa" = "#00FFFF", "Dfb" = "#38C7FF", "Dfc" = "#007E7D",
                               "ET" = "#B2B2B2"
                               )) +

  # scale_fill_manual(values = c("EBF" = "#BEF268", "DBF" = "#A6CC5C", "MF" = "#218D22", "ENF" = "#006501",
  # "WSA" = "#FE3130", "SAV" = "#8C1A1A", "CSH" = "#ff8300", "OSH" = "#cc4502",
  # "GRA" = "#C3FFC2", "CRO" = "#f3f725", "WET" = "#01BFFE")) +
  
  theme(legend.title=element_blank()) + # remove legend title (useless)
  theme(panel.border = element_rect(
    color = "black",
    fill = NA,   
    size = 1)
  ) +
  ylab("Number of sites") +
  xlab("Cluster") +
  ggtitle("Koeppen climate class") +
  scale_y_continuous(breaks = 1:22)
plot(o)

ggarrange(m, n, o,
          labels = c("a", "b", "c"),
          ncol = 2, nrow = 2
          # common.legend = TRUE, # have just one common legend
          # legend="bottom" # consider mettere legenda sotto a tutto con nomi IGBP spelled out
) 
# save plot
ggsave("extra.png", path = "./", width = 9, height = 8)










