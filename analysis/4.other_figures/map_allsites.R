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

# custom fonts
library(showtext)
font_add_google("Prata", regular.wt = 400)
showtext_auto()

# load table with all data from analysis
table1_final <- readRDS("data/table1_final.rds")

# get list of sites prelesected by beni (here is 128, in paper is 135, why???)
vector = c("AR-SLu", "AR-Vir", "AT-Neu", "AU-Ade", "AU-ASM", "AU-Cpr", "AU-Cum",
           "AU-DaP", "AU-DaS", "AU-Dry", "AU-Emr", "AU-Fog", "AU-Gin", "AU-GWW",
           "AU-How", "AU-RDF", "AU-Rob", "AU-Stp", "AU-Tum", "AU-Wac", "AU-Whr",
           "AU-Wom", "AU-Ync", "BE-Bra", "BE-Lon", "BE-Vie", "BR-Sa3", "CH-Cha",
           "CH-Dav", "CH-Fru", "CH-Lae", "CH-Oe1", "CH-Oe2", "CN-Cng", "CN-Dan",
           "CN-Din", "CN-Du2", "CN-Qia", "CZ-BK1", "CZ-BK2", "CZ-wet", "DE-Akm",
           "DE-Geb", "DE-Gri", "DE-Hai", "DE-Kli", "DE-Lkb", "DE-Obe", "DE-RuR",
           "DE-RuS", "DE-Seh", "DE-SfN", "DE-Spw", "DE-Tha", "DK-Fou", "DK-NuF",
           "DK-Sor", "ES-LgS", "ES-Ln2", "FI-Hyy", "FI-Jok", "FI-Sod", "FR-Fon",
           "FR-Gri", "FR-LBr", "FR-Pue", "GF-Guy", "IT-BCi", "IT-CA1", "IT-CA2",
           "IT-CA3", "IT-Col", "IT-Cp2", "IT-Cpz", "IT-Isp", "IT-La2", "IT-Lav",
           "IT-MBo", "IT-Noe", "IT-PT1", "IT-Ren", "IT-Ro1", "IT-Ro2", "IT-SR2",
           "IT-SRo", "IT-Tor", "JP-MBF", "JP-SMF", "NL-Hor", "NL-Loo", "NO-Adv",
           "RU-Fyo", "SD-Dem", "SN-Dhr", "US-AR1", "US-AR2", "US-ARb", "US-ARc",
           "US-ARM", "US-Blo", "US-Cop", "US-GLE", "US-Ha1", "US-Los", "US-Me2",
           "US-Me6", "US-MMS", "US-Myb", "US-Ne1", "US-Ne2", "US-Ne3", "US-ORv",
           "US-PFa", "US-SRG", "US-SRM", "US-Syv", "US-Ton", "US-Tw1", "US-Tw2",
           "US-Tw3", "US-Tw4", "US-Twt", "US-UMB", "US-UMd", "US-Var", "US-WCr",
           "US-Whs", "US-Wi0")

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

# map
# set robinson projection
robinson <- CRS("+proj=robin +over")

# download countries
countries <- ne_countries(scale = 50, returnclass = c("sf"))

# create a bounding box for the robinson projection
# we'll use this as "trim" to remove jagged edges at
# end of the map (due to the curved nature of the
# robinson projection)
bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = -180,
            xmax = 180,
            ymax = 90,
            ymin = -90), crs = st_crs(4326)),
  n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))

# transform the coastline to robinson
countries_robinson <- st_transform(countries, robinson)

# put points in right projection
points <- st_as_sf(table1_merged, coords = c("lon", "lat"), crs = 4326) # first put points in standard projection WGS84
points <- st_transform(points, crs = robinson) # then trasform to Robinson


# map
p1 <- ggplot() +
  geom_sf(data=countries_robinson, # countries borders
          color='grey25',
          linetype='solid',
          fill= "#cad6df",
          size=0.3) +
  geom_sf(data=bb_robinson, # box for Robinson projection
          color='black',
          linetype='solid',
          fill = NA,
          size=0.7) +
  theme_void() +
  geom_sf(data = points, # add EC sites
          size=1,
          aes(color = cluster, shape = cluster),) +
  scale_shape_manual(values = c("high fET" = 19,
                                "medium fET" = 19,
                                "low fET" = 19,
                                "excluded" = 4),
                     breaks = c("high fET",
                                "medium fET",
                                "low fET",
                                "excluded")) +
  # scale_fill_manual(values = c("high fET" = "#3A5ECC",
  #                              "medium fET" = "#e5aa44",
  #                              "low fET" = "#e55b39",
  #                              "excluded" = "#00FF01"),
  #                   breaks = c("high fET", # control order in legend
  #                              "medium fET",
  #                              "low fET",
  #                              "excluded")) +
  scale_color_manual(values = c("high fET" = "#3A5ECC",
                               "medium fET" = "#e5aa44",
                               "low fET" = "#e55b39",
                               "excluded" = "#00FF01"),
                    breaks = c("high fET", # control order in legend
                               "medium fET",
                               "low fET",
                               "excluded")) +
  theme(legend.title=element_blank(),
        legend.text=element_text(
          #lineheight = 0.3,
          color = "grey30",
          size=20,
          face="bold"),
        legend.position = c(.2, .6),
        plot.margin=unit(c(0.01,0.01,0.01,0.01), 'cm') # useless
        )
p1


# inset on Europe
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

# compose two maps
ggdraw(p1) +
  draw_plot(
    {p2},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0,
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.17,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.33,
    height = 0.33)


ggsave("beautiful_map.png", path = "./", width = 11)

