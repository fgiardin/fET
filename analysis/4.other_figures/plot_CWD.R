# script to visually display CWD timeseries for a sample site
# included in supplementary material

# load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)

# load df for US-Ton
load("data/output/US-Ton/data_frames/ddf_CWD_US-Ton.RData") # FLUXNET
load("data/output_GLDAS/US-Ton/GLDAS/ddf_CWD_gldas_US-Ton.RData") # GLDAS data

common_theme <- theme(
  axis.text=element_text(size = 12),
  axis.title=element_text(size = 14),
  plot.title = element_text(hjust = 0.5, size = 16, face="bold"), # center title
  plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm")
  )

# FLUXNET
out <- ddf_CWD
a <- ggplot() +
  geom_rect(
    data=out$inst,
    aes(xmin=date_start, xmax=date_end, ymin=-99, ymax=99999),
    fill="grey80",
    color=NA) +
  #geom_line(data = out$df, aes(date, P_F), size = 0.3, color="royalblue") +
  geom_line(data = out$df, aes(date, deficit), color="red") +
  #geom_line(data = out$df, aes(date, ET)) +
  coord_cartesian(ylim=c(0, 300)) +
  theme_classic() +
  labs(
    title = "FLUXNET2015",
    x = "Date (d)",
    y = "Cumulative water deficit (mm)") +
  scale_x_date(expand = c(0, 100)) + # remove blank space around origin
  scale_y_continuous(expand = c(0, 0)) +
  common_theme
a


# GLDAS
out <- ddf_CWD_gldas
b <- ggplot() +
  geom_rect(
    data=out$inst,
    aes(xmin=date_start, xmax=date_end, ymin=-99, ymax=99999),
    fill="grey80",
    color=NA) +
  #geom_line(data = out$df, aes(date, P_F), size = 0.3, color="royalblue") +
  geom_line(data = out$df, aes(date, deficit), color="red") +
  #geom_line(data = out$df, aes(date, ET)) +
  coord_cartesian(ylim=c(0, 300)) +
  theme_classic() +
  labs(
    title = "GLDAS-NOAH",
    x = "Date (d)",
    y = "Cumulative water deficit (mm)") +
  scale_x_date(expand = c(0, 100)) + # remove blank space around origin
  scale_y_continuous(expand = c(0, 0)) +
  common_theme
b


# create combined figure with subpanels
ggarrange(a, b,
          labels = c("a", "b"),
          ncol = 1, nrow = 2) # and place it

# save
ggsave("CWD_time_series.png", path = "./", width = 9, height = 8)


