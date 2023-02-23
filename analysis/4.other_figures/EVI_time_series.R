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

# define site to plot
site1_name = "AU-Stp"

#  Site 1 -----------------------------------------------------------------
load(paste0("data/output/", site1_name, "/data_frames/out_", site1_name, ".RData")) # load output of ML model
load(paste0("data/output/", site1_name, "/data_frames/ddf_", site1_name, ".RData")) # load input of ML model

# merge output and input dataframes
plottin <- out$df_all %>%
  left_join(ddf %>% dplyr::select(date, TA_F, NETRAD, EVI, VPD_F), by ="date") %>%
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

# calculate seasonality
site1 <- plottin %>%
  mutate(
    day_of_the_year = lubridate::yday(date)
  ) %>%
  group_by(day_of_the_year) %>%
  summarise(
    nn_pot = mean(nn_pot, na.rm = TRUE),
    nn_act = mean(nn_act, na.rm = TRUE),
    fvar = mean(fvar, na.rm = TRUE),
    obs = mean(obs, na.rm = TRUE),
    netrad = mean(NETRAD_coeff, na.rm = TRUE),
    moist = mean(moist, na.rm = TRUE),
    pet_SPLASH = mean(pet_splash_coeff, na.rm = TRUE),
    EVI = mean(EVI, na.rm = TRUE),
    VPD_F = mean(VPD_F, na.rm = TRUE)
  ) %>%
  mutate(
    date = as.Date(paste(2000, day_of_the_year), "%Y %j"),
    month = format(date, "%b")) %>%
  ungroup()

# calculate coefficient to scale EVI and fvar in double axis (so that maxima of fvar and nn_pot are the same)
coeff <- max(site1$fvar, na.rm = TRUE)/max(site1$nn_pot, na.rm = TRUE)

# long format
df_site1 <- site1 %>%
  mutate(EVI_coeff = EVI/coeff) %>%
  mutate(fvar_coeff = fvar/coeff) %>%
  pivot_longer(
    cols = c(nn_pot, nn_act, obs, netrad, pet_SPLASH, EVI, VPD_F, fvar, fvar_coeff, EVI_coeff),
    names_to = "names",
    values_to = "values"
  ) %>%
  mutate(moist = as.logical(moist)) %>%
  mutate(dry = !moist)


### PLOT
# annotation
grob_a <- grobTree(textGrob(site1_name, x=0.01,  y=0.95, hjust=0,
                            gp=gpar(col="black", fontsize=14, fontface="bold")))

# calculate maximum to set axis limit
max_pot = df_site1 %>% dplyr::filter(names == "nn_pot") %>% pull(values) %>% max(na.rm = TRUE)
max_pot = max_pot + 0.1*max_pot

# plot
a <- ggplot(data = df_site1) +
  geom_path(
    aes(
      date,
      values,
      color = names,
      group = names,
      linetype = names,
    ),
    size=0.6
  ) +
  scale_y_continuous(
    name = expression(paste("ET (mm ", d^-1, ")")), # Features of the first axis
    limits = c(0,max_pot),
    sec.axis = sec_axis(~.*coeff, name="fET and EVI (-)")  # Add a second axis and specify its features
  ) +
  labs(
    x = "Month"
  ) +
  theme_classic() +
  scale_color_manual(  # set line colors
    values = c(obs = "#333333",
               fvar_coeff = "#2353ce", # blue
               nn_pot = "#D81B60", # red
               EVI_coeff = "#4aaf2b"), # green
    labels = c(obs = expression(paste(ET[obs])), # set labels for legend
               fvar_coeff = expression(paste(fET)),
               nn_pot = expression(paste(PET[NN])),
               EVI_coeff = expression(paste(EVI))
    )
  ) +
  scale_linetype_manual(  # set line types
    values = c(obs = "solid",
               fvar_coeff = "solid",
               nn_pot = "solid",
               EVI_coeff = "solid"
    ),
    guide = "none" # hide legend for lines
  ) +
  scale_x_date(date_breaks="1 month", date_labels = "%b") + # set correct x axis
  annotation_custom(grob_a) +
  theme( # set legend position and orientation, as well as text size
    legend.title=element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14),
    legend.text=element_text(size = 12)
  )
plot(a)

# save
ggsave(paste0("EVI_time_series_", site1_name, ".png"), path = "./", width = 9, height = 4)










