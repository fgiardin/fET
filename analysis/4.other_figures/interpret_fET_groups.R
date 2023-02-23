# Code to plot current figure 6 to interpret cluster and see how other
# variables (aridity index, soil texture, etc.) vary across fET groups

#Load packages
devtools::load_all(".")
library(tidyverse)
library(ggpubr)
library(ingestr)
library(grid)
library(plotrix)

# load table with already-extracted soil texture data
# produced by running analysis/3.summary_plots.R to create
# table1_raw.RData and then run data-raw/extract_HWSD.R
load("data-raw/table1_soil.RData")

# load vector of final sites
load("data/dataframes/vec_sites.RData")

# only select sites that are in final analysis
table1 <- table1 %>%
  dplyr::filter(name_site %in% vec_sites)

# extract median fvar per site, calculated in the central CWD bin (see methods)
load("data/dataframes/plot_allsites_fvar.RData")
median_fvar <- plot_allsites_fvar %>%
  dplyr::select(name_site, median_fvar) %>%
  unique()

# append median fvar
table1 <- table1 %>%
  left_join(median_fvar, by = "name_site")

# reverse order of factors (so that high fET is first, consistently with fET vs CWD figures)
table1$cluster <- factor(table1$cluster, levels=rev(levels(table1$cluster)))


### PANEL A0: Soil texture triangle plot ####################################
texture_plot <- table1 %>%
  dplyr::select(name_site, T_CLAY, T_SAND, T_SILT, cluster) %>%
  na.omit() %>%
  mutate(color = cluster)

# create colors
texture_plot$color <- gsub('high fET', '#e9c46a', texture_plot$color)
texture_plot$color <- gsub('medium fET', '#f4a261', texture_plot$color)
texture_plot$color <- gsub('low fET', '#e76f51', texture_plot$color)
colors = c("#e9c46a", "#f4a261", "#e76f51") # for legend

soil.texture(texture_plot[,2:4],
             # label.points = TRUE, # show labels
             # point.labels = texture_plot$cluster,
             show.names = FALSE,
             show.lines = FALSE,
             show.grid = TRUE,
             col.symbols = texture_plot$color,
             bg.symbols = texture_plot$color,
             pch=19)
legend(x=1,
       legend = c("high fET", "medium fET", "low fET"),
       col = colors,
       fill = colors)

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

# subsoil (not used)
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

# uncomment to save it
#ggsave("soil_texture_subsoil.png", path = "./", width = 9, height = 8)

### PANEL B: IGBP Class ####################################
# adjust table
table1 <- table1 %>%
  mutate(classid = factor(classid, # order IGBP classes (consistent with Fig. 1 of fluxnet2015 main paper)
                          levels = c("EBF", "DBF", "MF", "ENF",
                                     "WSA", "SAV", "OSH", "CSH",
                                     "GRA", "CRO", "WET")))

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

# load aridity index values (see Methods)
load("data-raw/ai_fluxnet2015.Rdata")
df_ai <- df_ai %>%
  rename(name_site = mysitename)

# merge with table
table1 <- table1 %>%
  left_join(df_ai %>% dplyr::select(name_site, ai), by = "name_site")

# aridity index (from np2018)
c <- ggplot(table1, aes(x = cluster, y = ai, fill = cluster)) +
  stat_boxplot(geom ='errorbar', width = 0.5) + # add error bars to whiskers
  geom_boxplot(width = 0.5) +
  theme_classic() +
  ylab("Aridity index (-)") +
  theme(
        axis.title.x=element_blank(), # remove text of x axis
        axis.text.y=element_text(size = 14), # adjust text size and color
        axis.text.x=element_text(size = 14, color = "black"),
        axis.title=element_text(size = 16),
        legend.text=element_blank(),
        legend.position="none",
        panel.border = element_rect(  # box around figure
          color = "black",
          fill = NA,
          size = 1),
        axis.line=element_blank(),  # remove axis line (otherwise overlap with box)
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm") # set plot margins (useful when creating combined figure)
        ) +
  scale_fill_manual(values = c("#e9c46a", "#f4a261", "#e76f51")) +  # set colors
  scale_y_continuous(breaks = seq(0, 2, 0.25)) # adjust y axis ticks
plot(c)

### PANEL D: global topographic index (GTI) ###################
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis

# load df with gti already extracted at site locations
load("data-raw/df_gti.RData")

# merge with table
table1 <- table1 %>%
  left_join(df_gti %>% dplyr::select(name_site, gti), by = "name_site")

d <- ggplot(df_gti, aes(x= cluster, y = gti, fill = cluster)) +
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot(width = 0.5) +
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
  scale_y_continuous(breaks = seq(0, 10, 2)) # change y axis label (but not limits)
plot(d)

### PANEL E-F: MAP and MAT ####################################
## used worldclim data for MAP and MAT since most Australian sites don't have MAP/MAT in
## the fluxnet dataframe

# load worldclim dataframe and merge to our table
load("data-raw/df_WorldClim.RData")

table1 <- table1 %>%
  left_join(df_worldclim_reformat, by = c("name_site"))

# MAP
e <- ggplot(table1, aes(x= cluster, y = MAP_worldclim, fill = cluster)) +
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot(width = 0.5) +
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
  stat_boxplot(geom ='errorbar', width = 0.5) +
  geom_boxplot(width = 0.5) +
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

# remove site duplicates
table1 <- table1 %>%
  unique()

## SAVE FINAL TABLE1
saveRDS(table1, "./table1_final.rds", compress = "xz")

# create fET timeseries
load("data/dataframes/plot_allsites_fvar.RData")

fET_timeseries <- plot_allsites_fvar %>%
  left_join(table1, by = c("name_site", "median_fvar", "cluster")) %>%
  unique()

## SAVE fET timeseries
saveRDS(fET_timeseries, "./fET_timeseries.rds", compress = "xz")

# save in CSV format as well
write.csv(fET_timeseries,"./fET_timeseries.csv", row.names = FALSE)


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

# Notes on the statistical significance of the slopes
# We use 't' critical value from performing t-test on the slope using alpha = 0.05.
# If the p-value is smaller than 0.05, the data (x) is successfully explaining the variance in y
# ==> We can reject the null hypothesis that the slope is equal to zero

### CLAY
# split in equal-sized groups based on soil fraction
clay_groups <- quantile(table1$T_CLAY, c(.33, .67), na.rm = TRUE)

# fit a regression model in every group
model_1 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_CLAY < clay_groups[1])) # forcing intercept = 0, otherwise regression lines won't be comparable
model_2 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_CLAY > clay_groups[1]) %>% dplyr::filter(T_CLAY < clay_groups[2]))
model_3 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_CLAY > clay_groups[2]))

# create string for every slope value
title_a = sprintf("Slope = %.2f", summary(model_1)$coefficients[,1])
title_b = sprintf("Slope = %.2f", summary(model_2)$coefficients[,1])
title_c = sprintf("Slope = %.2f", summary(model_3)$coefficients[,1] )

# print p-values (have to be lower than 0.05)
summary(model_1)$coefficients[,4]
summary(model_2)$coefficients[,4]
summary(model_3)$coefficients[,4]

# create annotation to add to the plot for every string above
grob_a <- grobTree(textGrob(title_a, x=0.02,  y=0.95, hjust=0,
                            gp=gpar(col="#0041a3", fontsize=14, fontface="bold")))
grob_b <- grobTree(textGrob(title_b, x=0.02,  y=0.89, hjust=0,
                            gp=gpar(col="#009246", fontsize=14, fontface="bold")))
grob_c <- grobTree(textGrob(title_c, x=0.02,  y=0.83, hjust=0,
                            gp=gpar(col="#CE2B37", fontsize=14, fontface="bold")))

# plot
v <- ggplot(table1) +
  geom_point(aes(x = ai, y = median_fvar, size = T_CLAY)) +
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
model_1 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SILT < silt_groups[1])) # forcing intercept = 0, otherwise regression lines won't be comparable
model_2 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SILT > silt_groups[1]) %>% dplyr::filter(T_SILT < silt_groups[2]))
model_3 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SILT > silt_groups[2]))

title_a = sprintf("Slope = %.2f", summary(model_1)$coefficients[,1])
title_b = sprintf("Slope = %.2f", summary(model_2)$coefficients[,1])
title_c = sprintf("Slope = %.2f", summary(model_3)$coefficients[,1] )

# print p-values
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
  geom_point(aes(x = ai, y = median_fvar, size = T_SILT)) +
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

model_1 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SAND < sand_groups[1])) # forcing intercept = 0, otherwise regression lines won't be comparable
model_2 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SAND > sand_groups[1]) %>% dplyr::filter(T_SAND < sand_groups[2]))
model_3 = lm(median_fvar ~ ai + 0, table1 %>% dplyr::filter(T_SAND > sand_groups[2]))

title_a = sprintf("Slope = %.2f", summary(model_1)$coefficients[,1])
title_b = sprintf("Slope = %.2f", summary(model_2)$coefficients[,1])
title_c = sprintf("Slope = %.2f", summary(model_3)$coefficients[,1] )

# p-values
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
  geom_point(aes(x = ai, y = median_fvar, size = T_SAND)) +
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











