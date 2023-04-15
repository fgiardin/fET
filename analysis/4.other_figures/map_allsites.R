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

# CWDX80 data -------------------------------------------------------------

# read the data
df <- nc_to_df("data-raw/CWDX80/cwdx80_tenthdeg.nc", varnam = "cwdx80") |>
  mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2))

## Define bins of color scale
breaks <- c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf)

## Define bins of color scale
breaks <- c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf)

## define domain object
domain <- c(lonmin, lonmax, latmin, latmax)

## resize data frame to limit data volume (geographic clipping is done separately)
df <- df |>
  dplyr::filter(lon > domain[1] & lon < domain[2] & lat > domain[3] & lat < domain[4])

nbin <- length(breaks) - 1
breaks_with <- breaks

## Indicate that color scale extends to minus or plus infinity by a triangle
toptriangle <- FALSE
bottomtriangle <- FALSE
if (is.infinite(breaks[length(breaks)])){
  toptriangle <- TRUE
  breaks <- breaks[-(length(breaks)-1)]
}
if (is.infinite(breaks[1])){
  bottomtriangle <- TRUE
  breaks <- breaks[-2]
}
nbin <- length(breaks) - 1

## add dummy rows to make sure values in layer span the entire range
df <- df |>
  bind_rows(
    tibble(
      lon = NA,
      lat = NA,
      cwdx80 = breaks[1:(length(breaks)-1)] + 0.5 * (breaks[2]-breaks[1])
    )
  )

## make the data discrete
df$layercut <- as.factor(base::cut(df$cwdx80, breaks = breaks, labels = FALSE, include.lowest = TRUE))

## Define colors
colorscale <- scico::scico(nbin, palette = "batlowK", direction = -1)

## Define color of the top and bottom triangle if required
if (toptriangle){
  colorscale <- c(colorscale, colorscale[length(colorscale)])
}
if (bottomtriangle){
  colorscale <- c(colorscale[1], colorscale)
}

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
  scale = 10,
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

df_plot <- df %>%
  drop_na() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # convert to robinson projection
  st_transform(crs = robinson) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1], # convert back to df for plotting
                lat = sf::st_coordinates(.)[,2])


# land = lengths(st_intersects(df_plot, ocean_robinson)) > 0 # force intersection between points and USA borders
# df_plot <- df_plot %>%
#   dplyr::filter(land)

# map ---------------------------------------------------------------------

p1 <- ggplot() +
  theme_void() +
  geom_sf(data=bb_robinson, # box for Robinson projection
          color='black',
          linetype='solid',
          fill = "#7F7F7F", # "lightblue", #"grey75",#"#D6D6E9",
          size=0.1) +
  # geom_sf(data = df_plot,
  #         # size=0.1,
  #         aes(fill = layercut, color = layercut),
  #         show.legend = FALSE) +
  geom_tile(
    data = df_plot,
    aes(x = lon, y = lat, fill = layercut, color = layercut),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = colorscale) +
  scale_color_manual(values = colorscale) +
  geom_sf(data=ocean_robinson,
          colour='grey23',
          linetype='solid',
          fill = "white", # "lightblue" "
          size=0.2) +
  geom_sf(data=bb_robinson, # box for Robinson projection
          color='black',
          linetype='solid',
          fill = NA, # "lightblue", #"grey75",#"#D6D6E9",
          size=0.1) +
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
                                "excluded" = "white"), #"#00FF01"
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
    legend.background = element_rect(fill="grey70", color = NA),
    legend.position = c(.2, .2),
    legend.key.size = unit(0.1, 'lines'), # to change vertical spacing in legend (default is too much)
    plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'))
ggsave("legend.png", path = "./", width = 11) # save this for the legend


# inset on Europe
p2 <- ggplot() +
  theme_void() +
  coord_sf( # define area
    xlim = c(-5, 20),
    ylim = c(36, 58),
    expand = FALSE,
    st_crs(4326)
  ) +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=0.3)) + # add border
  geom_tile(
    data = df %>%
      dplyr::filter(lon > -5,
                    lon < 20,
                    lat > 36,
                    lat < 58),
    aes(x = lon, y = lat, fill = layercut, color = layercut),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = colorscale, na.value = "#7F7F7F") +
  scale_color_manual(values = colorscale, na.value = "#7F7F7F") +
  geom_sf(data=ocean,
          colour='grey23',
          linetype='solid',
          fill = "white", # "lightblue" "
          size=0.2) +
  new_scale_color() + # add new color scales for map
  geom_sf(data = points, # add EC sites
          size=2,
          aes(color = factor(cluster), shape = factor(cluster), fill = factor(cluster))) +
  coord_sf( # cut back to area
    xlim = c(-5, 20),
    ylim = c(36, 58),
    expand = FALSE,
    st_crs(4326)
  ) +
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
                                "excluded" = "white"), #"#00FF01"
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
    legend.background = element_rect(fill="grey70", color = NA),
    legend.position = "none",
    legend.key.size = unit(0.1, 'lines'), # to change vertical spacing in legend (default is too much)
    plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'))

# # inset on USA
# p4 <- p1 +
#   coord_sf( # define area
#     xlim = c(-125, -65),
#     ylim = c(24, 50),
#     expand = FALSE,
#     st_crs(4326)
#   ) +
#   theme(legend.position = "none",
#         panel.border = element_rect(colour = "black", fill=NA, size=0.3)) # add border
# p4

# compose two maps
p3 <- ggdraw(
  p1 +
    theme(legend.position = "none")) + # remove legend (the settings are not imported correctly in cowplot)
  draw_plot(
    {p2},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = -0.03,
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.12,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.37,
    height = 0.37)

## use the function obtained from https://github.com/adrfantini/plot_discrete_cbar
## here, it's in the directory ./R/
gglegend <- plot_discrete_cbar(
  breaks           = breaks_with, # Vector of breaks. If +-Inf are used, triangles will be added to the sides the color bar
  colors           = colorscale,
  legend_direction = "horizontal",
  spacing = "constant",
  expand_size_y = 0.4,
  width = 0.04,
  font_size = 11,  # of color key labels
  triangle_size = 0.08,
)

## Combine map and legend with cowplot
cowplot::plot_grid(p3, gglegend, nrow = 2, rel_heights = c(1, 0.2))

ggsave("beautiful_map.png", path = "./", width = 11)

# save legend separately (for CWD label)
gglegend <- plot_discrete_cbar(
  breaks           = breaks_with, # Vector of breaks. If +-Inf are used, triangles will be added to the sides the color bar
  colors           = colorscale,
  legend_direction = "vertical",
  spacing = "constant",
  expand_size_y = 0.4,
  width = 0.06,
  font_size = 20,  # of color key labels
  triangle_size = 0.1,
  legend_title     = expression(paste(CWD[X80], " (mm)")),
)
gglegend
ggsave("legend_CWD.png", path = "./", width = 11)

