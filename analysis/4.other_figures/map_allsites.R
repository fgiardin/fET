# Script to print map of all sites

# load packages
devtools::load_all(".")
library(tidyverse)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(cowplot)
library(ingestr)
library(terra)
library(rgdal)  # for readOGR
library(ggnewscale) # to add multiple scales in same ggplot
library(gridExtra)

sf_use_s2(FALSE)

# custom fonts
library(showtext)
font_add_google("Prata", regular.wt = 400)
showtext_auto()

# define extent of the map
latmin <- -60
latmax <- 88
lonmin <- -179.999
lonmax <- 179.999

# fET points data  --------------------------------------------------------

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


# prepare map ---------------------------------------------------------------------
# set robinson projection
robinson <- CRS("+proj=robin +over")

# create a bounding box for the robinson projection
# we'll use this as "trim" to remove jagged edges at
# end of the map (due to the curved nature of the
# robinson projection)
bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = lonmin,
            xmax = lonmax,
            ymax = latmax,
            ymin = latmin), crs = st_crs(4326)),
  n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))


## read 110 m resolution coastline from NaturalEarth data
coast <- ne_coastline(scale = 50, returnclass = "sf")
coast_robinson <- coast %>%
  st_buffer(0) %>%
  st_intersection(st_union(bb)) %>% # cut to match bb
  st_transform(as.character(robinson)) # transform to robinson

# download ocean outlines
ocean <- ne_download(
  scale = 50,
  type = "ocean",
  category = "physical",
  returnclass = "sf")
ocean_robinson <- ocean %>%
  st_buffer(0) %>%
  st_intersection(st_union(bb)) %>% # cut to match bb
  st_transform(robinson)


# put points in right projection
points <- st_as_sf(table1_merged, coords = c("lon", "lat"), crs = 4326) # first put points in standard projection WGS84
points_robinson <- st_transform(points, crs = robinson) # then trasform to Robinson


# map ---------------------------------------------------------------------

lwidth = 0.2 # witdh of contour lines

# world map
p1 <- ggplot() +
  theme_void() +
  geom_sf(data=bb_robinson, # box for Robinson projection
          color='black',
          linetype='solid',
          fill = "#F2F2F2", # #D3D3D3 "lightblue", # LAND
          size=0.3) +
  geom_sf(data=ocean_robinson,
          colour='black',
          linetype='solid',
          fill = "white", # #eff8ff # OCEAN
          linewidth=lwidth) +
  geom_sf(data=bb_robinson, # box for Robinson projection
          color='black',
          linetype='solid',
          fill = NA,
          linewidth=lwidth) +
  new_scale_color() + # add new color scales for map
  geom_sf(data = points_robinson, # add EC sites
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
                               "low fET" = "#e55b39",
                               "excluded" = "black"),
                    breaks = c("high fET", # control order in legend
                               "medium fET",
                               "low fET",
                               "excluded")) +
  theme(
    legend.title=element_blank(),
    legend.text=element_text(color = "black", size=40, family = "Prata"),
    legend.background = element_rect(fill="white", color = "black", linewidth = lwidth), # add black border
    legend.position = c(.11, .59),
    legend.key.size = unit(0.1, 'lines'),
    plot.margin=unit(c(0,0,0,0), 'cm'),
    legend.spacing.y = unit(0, 'cm'), # remove blank space on top of legend
    legend.margin = margin(1, 1, 1, 1) # margin around the legend
    )
ggsave("world.png", path = "./", width = 11, dpi = 600)


# inset on Europe
p2 <- p1 +
  coord_sf( # define area
    xlim = c(-5, 20),
    ylim = c(36, 58),
    expand = FALSE,
    st_crs(4326)
  ) +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill= NA, linewidth=lwidth+0.3)) # add border
ggsave("europe.png", path = "./", width = 2, dpi = 1000) # higher res to make sure it keeps it when pasting in figure above




