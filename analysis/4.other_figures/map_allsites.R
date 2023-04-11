# Script to print map of all sites

# load packages
library(tidyverse)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(cowplot)
library(ingestr)
library(terra)

# custom fonts
library(showtext)
font_add_google("Prata", regular.wt = 400)
showtext_auto()

# load table with all data from analysis
table1_final <- readRDS("data/table1_final.rds")

# get list of 135 sites out of 166 considered in study (same as np2018)
soil_usability <- read.csv("data/soilm_data_usability_fluxnet2015.csv") %>%
  dplyr::filter(code != 0)
vector <- soil_usability$mysitename

# get coordinates of these sites
ciaone <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(sitename %in% vector) %>%
  dplyr::select(sitename, lon, lat, classid) %>%
  rename(name_site = sitename) %>%
  dplyr::filter(!name_site %in% unique(table1_final$name_site)) # exclude final list of sites retained for NN
                                                                # to avoid repetitions when merging below

table1_merged <- dplyr::bind_rows(ciaone, table1_final) # bind rows
table1_merged$cluster <- as.character(table1_merged$cluster) %>% # convert to character so that replace_na works
  replace_na("excluded") %>% # rename NAs
  factor(levels = c("excluded", "low fET", "medium fET", "high fET")) # put back factors


# map ---------------------------------------------------------------------

# set robinson projection
robinson <- CRS("+proj=robin +over")

# download countries
countries <- ne_countries(scale = 50, returnclass = c("sf"))

# transform the coastline to robinson
countries_robinson <- st_transform(countries, robinson)

# download ocean outlines
ocean <- ne_download(
  scale = 50,
  type = "ocean",
  category = "physical",
  returnclass = "sf")
ocean_robinson <- st_transform(ocean, robinson)

# create a bounding box for the robinson projection
# we'll use this as "trim" to remove jagged edges at
# end of the map (due to the curved nature of the
# robinson projection)
bb <- sf::st_union(
  sf::st_make_grid(
    st_bbox(
      c(xmin = -180,
        xmax = 180,
        ymax = 90,
        ymin = -90),
      crs = st_crs(4326)),
    n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))

# put points in right projection
points <- st_as_sf(table1_merged, coords = c("lon", "lat"), crs = 4326) # first put points in standard projection WGS84
points <- st_transform(points, crs = robinson) # then trasform to Robinson

crop <- data.frame(
  c(-180, 180),
  c(-60, 88))
names(crop) <- c("lon", "lat")
crop <- st_as_sf(crop, coords = c("lon", "lat"), crs = 4326)
crop <- st_transform(crop, crs = robinson)



# map
p1 <- ggplot() +
  geom_sf(data=bb_robinson, # box for Robinson projection
          color='black',
          linetype='solid',
          fill = "white", # "lightblue", #"grey75",#"#D6D6E9",
          size=0.1) +
  # geom_sf(data=countries_robinson, # country borders
  #         color= "grey23",
  #         linetype='solid',
  #         fill= "white",#cad6df", #D6D6E9
  #         size=0.3) +
  geom_sf(data = ocean_robinson, # add oceans
          color = "grey23",
          fill = "lightblue",
          size = 0.2) +
  theme_void() +
  geom_sf(data = points, # add EC sites
          size=2,
          aes(color = factor(cluster), shape = factor(cluster), fill = factor(cluster))) +
  scale_shape_manual(values = c("high fET" = 21,
                                "medium fET" = 21,
                                "low fET" = 21,
                                "excluded" = 4),
                     breaks = c("high fET",
                                "medium fET",
                                "low fET",
                                "excluded")) +
  scale_color_manual(values = c("high fET" = "black",
                                "medium fET" = "black", #"#f6c59d", #"#e2dd44",
                                "low fET" = "black",
                                "excluded" = "black"), #"#00FF01"
                     breaks = c("high fET", # control order in legend
                                "medium fET",
                                "low fET",
                                "excluded")) +
  scale_fill_manual(values = c("high fET" = "#3A5ECC",
                               "medium fET" = "#F6C59D",
                               "low fET" = "#e55b39"),
                    breaks = c("high fET", # control order in legend
                               "medium fET",
                               "low fET")) +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  theme(
    legend.title=element_blank(),
    legend.text=element_text(color = "black", size=23, family = "Prata"),
    legend.background = element_rect(fill="white", color = NA),
    legend.position = c(.2, .2),
    legend.key.size = unit(0.1, 'lines'), # to change vertical spacing in legend (default is too much)
    plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'))

p1
ggsave("legend.png", path = "./", width = 11) # save this for the legend



# inset on Europe
sf_use_s2(FALSE)
p2 <- p1 +
  coord_sf( # define area
    xlim = c(-5, 20),
    ylim = c(36, 58),
    expand = FALSE,
    st_crs(4326)
  ) +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=0.3)) # add border
p2

# inset on USA
p4 <- p1 +
  coord_sf( # define area
    xlim = c(-125, -65),
    ylim = c(24, 50),
    expand = FALSE,
    st_crs(4326)
  ) +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=0.3)) # add border
p4

# compose two maps
p3 <- ggdraw(
  p1 +
    theme(legend.position = "none")) + # remove legend (the settings are not imported correctly in cowplot)
  draw_plot(
    {p2},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = -0.03,
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.13,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.37,
    height = 0.37)
p3

ggsave("beautiful_map.png", path = "./", width = 11)

