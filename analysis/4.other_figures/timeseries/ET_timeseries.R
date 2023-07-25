# script to plot timeseries of ET

# load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(grid)
library(bigleaf)

# load dataframes
scatter_plots_all <- readRDS("data/dataframes/scatter_plots_all.rds")
scatter_plots_all_selected <- scatter_plots_all %>%
  dplyr::select(date, name_site, pet_splash_coeff, NETRAD_mass_coeff)

### define sites
site1_name = "DK-Sor"
site2_name = "US-Ton"

### Site 1
load(paste0("data/output/", site1_name, "/data_frames/out_", site1_name, ".RData")) # load output of ML model
load(paste0("data/output/", site1_name, "/data_frames/ddf_", site1_name, ".RData")) # load input of ML model

# merge output and input dataframes
plottin <- out$df_all %>%
  left_join(ddf %>% dplyr::select(date, TA_F, NETRAD), by ="date") %>%
  mutate(NETRAD = LE.to.ET(NETRAD, TA_F)*24*60*60)  # convert NETRAD to mass units (same of ET -- kg/m2*d)

# get coeff to scale netrad
lm_model = lm(nn_pot ~ NETRAD + 0, plottin)
coeff = lm_model[["coefficients"]][["NETRAD"]]

# calculate scaled netrad and add PET from SPLASH
plottin <- plottin %>%
  mutate(NETRAD_coeff = NETRAD*coeff) %>%
  left_join(
    scatter_plots_all_selected %>% dplyr::filter(name_site == site1_name),
    by = c("date")
  )

# rename df and create column for long format
site_1 <- plottin
site_1$site <- site1_name


### Site 2
load(paste0("data/output/", site2_name, "/data_frames/out_", site2_name, ".RData")) # load output of ML model
load(paste0("data/output/", site2_name, "/data_frames/ddf_", site2_name, ".RData")) # load input of ML model


# merge output and input dataframes
plottin <- out$df_all %>%
  left_join(ddf %>% dplyr::select(date, TA_F, NETRAD), by ="date") %>%
  mutate(NETRAD = LE.to.ET(NETRAD, TA_F)*24*60*60)  # convert NETRAD in same units of ET (kg/m2*d)

# get coeff to scale netrad
lm_model = lm(nn_pot ~ NETRAD + 0, plottin)
coeff = lm_model[["coefficients"]][["NETRAD"]]

# calculate scaled netrad
plottin <- plottin %>%
  mutate(NETRAD_coeff = NETRAD*coeff) %>%
  left_join(
    scatter_plots_all_selected %>% dplyr::filter(name_site == site2_name),
    by = c("date")
  )

# rename df and create column for long format
site_2 <- plottin
site_2$site <- site2_name

# combine both sites, with a site column
df_raw <- bind_rows(site_1, site_2)

# calculate seasonality
df <- df_raw %>%
  mutate(
    day_of_the_year = lubridate::yday(date)
  ) %>%
  group_by(site, day_of_the_year) %>%
  summarise(
    nn_pot = mean(nn_pot, na.rm = TRUE),
    nn_act = mean(nn_act, na.rm = TRUE),
    fvar = mean(fvar, na.rm = TRUE),
    obs = mean(obs, na.rm = TRUE),
    netrad = mean(NETRAD_coeff, na.rm = TRUE),
    moist = mean(moist, na.rm = TRUE),
    pet_SPLASH = mean(pet_splash_coeff, na.rm = TRUE)
  ) %>%
  mutate(
    date = as.Date(paste(2000, day_of_the_year), "%Y %j"),
    month = format(date, "%b")
  ) %>%
  group_by(site) %>%
  # pivot wide data to long data with the names
  # of the columns assigned to "names" and the
  # values to "value"
  pivot_longer(
    cols = c(nn_pot, nn_act, obs, netrad, pet_SPLASH),
    names_to = "names",
    values_to = "value"
  ) %>%
  ungroup() %>%
  mutate(moist = as.logical(moist)) %>%
  mutate(dry = !moist)

# annotation
grob_a <- grobTree(textGrob(site1_name, x=0.01,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=14, fontface="bold")))


# plot
a <- ggplot(data = df %>% dplyr::filter(site == site1_name)) +
  geom_path(
    aes(
      date,
      value,
      color = names,
      group = names,
      linetype = names,
    ),
    size=0.6
  ) +
  labs(
    x = "Month",
    y = expression(paste("ET (mm ", d^-1, ")"))
  ) +
  theme_classic() +
  theme(legend.title=element_blank()) +
  scale_color_manual(  # set line colors
    values = c(obs = "#333333",
               nn_act = "#0072B2", # blue
               nn_pot = "#D81B60", # red
               netrad = "#BBCC33"), # green
    labels = c(obs = expression(paste(ET[obs])), # set labels for legend
               nn_act = expression(paste(ET[NN])),
               nn_pot = expression(paste(PET[NN])),
               netrad = "Net Radiation"
    )
  ) +
  scale_linetype_manual(  # set line types
    values = c(obs = "solid",
               nn_act = "solid",
               nn_pot = "solid",
               netrad = "dashed"
    ),
    guide = "none" # hide legend for lines
  ) +
  scale_x_date(date_breaks="1 month", date_labels = "%b", expand = c(0.003, 0)) + # set correct x axis
  annotation_custom(grob_a) +
  theme( # set legend position and orientation, as well as text size
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14),
    legend.text=element_text(size = 12)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.1, 4.3))
plot(a)

grob_b <- grobTree(textGrob(site2_name, x=0.01,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=14, fontface="bold")))
b <- ggplot(data = df %>% dplyr::filter(site == site2_name)) +
  geom_path(
    aes(
      date,
      value,
      color = names,
      group = names,
      linetype = names,
    ),
    size=0.6
  ) +
  labs(
    x = "Month",
    y = expression(paste("ET (mm ", d^-1, ")"))
  ) +
  theme_classic() +
  theme(legend.title=element_blank()) +
  scale_color_manual(  # set line colors
    values = c(obs = "#333333",
               nn_act = "#0072B2", # blue
               nn_pot = "#D81B60", # red
               netrad = "#BBCC33"), # green
    labels = c(obs = expression(paste(ET[obs])), # set labels for legend
               nn_act = expression(paste(ET[NN])),
               nn_pot = expression(paste(PET[NN])),
               netrad = "Net Radiation")) +
  scale_linetype_manual(
    values = c(obs = "solid",
               nn_act = "solid",
               nn_pot = "solid",
               netrad = "dashed"
    ),
    guide = "none") +
  scale_x_date(date_breaks="1 month", date_labels = "%b", expand = c(0.002, 0)) +
  annotation_custom(grob_b) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14),
    legend.text=element_text(size = 12)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.1, 4.3))
plot(b)

# create combined figure with subpanels
ggarrange(a, b,
          labels = c("a", "b"),
          ncol = 1, nrow = 2,
          common.legend = TRUE, # have just one common legend
          legend="top") # and place it

# save
ggsave("ET_time_series.png", path = "./", width = 9, height = 8, dpi = 600)
ggsave("ET_time_series.pdf", path = "./", width = 9, height = 8, dpi = 600)

