### script to look into details at some dry sites (reply to reviews)

# load packages
devtools::load_all(".")
library(tidyverse)
library(ggpubr)
source("~/fET/R/LSD.heatscatter.R")

# load dataframes
scatter_plots_all <- readRDS("data/dataframes/scatter_plots_all.rds")

# scatter plots -----------------------------------------------------------

# SPLASH
# AU-Stp
file = sprintf("./scatter_nn_pot-splash_allsites_AU-Stp.png")
scatterheat(scatter_plots_all %>% dplyr::filter(name_site == "AU-Stp"), "nn_pot", "pet_splash_coeff", "All days, AU-Stp", file)

# AU-Cpr
file = sprintf("./scatter_nn_pot-splash_allsites_AU-Cpr.png")
scatterheat(scatter_plots_all %>% dplyr::filter(name_site == "AU-Cpr"), "nn_pot", "pet_splash_coeff", "All days, AU-Cpr", file)


# NETRAD
# AU-Stp
file = sprintf("./scatter_nn_pot-netrad_allsites_AU-Stp.png")
scatterheat(scatter_plots_all %>% dplyr::filter(name_site == "AU-Stp"), "nn_pot", "NETRAD_mass_coeff", "All days, AU-Stp", file)

# AU-Cpr
file = sprintf("./scatter_nn_pot-netrad_allsites_AU-Cpr.png")
scatterheat(scatter_plots_all %>% dplyr::filter(name_site == "AU-Cpr"), "nn_pot", "NETRAD_mass_coeff", "All days, AU-Cpr", file)



# density plots -----------------------------------------------------------

# load data and function
load("data/dataframes/plot_allsites_fvar.RData")
source("~/fET/R/LSD.heatscatter.R")

# AU-Stp
df <- plot_allsites_fvar %>% dplyr::filter(name_site == "AU-Stp")
a <- heatscatter(x=df$deficit, y = df$fvar, ggplot = TRUE)
a$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
a <- a +
  labs(
    title = "AU-Stp",
    y = "fET (-)",
    x = "Cumulative water deficit (mm)") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 17),
    axis.title=element_text(size = 19),
    strip.text = element_text(size=17) # plot title of facet_wrap multiplot
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300))
a

# AU-Cpr
df <- plot_allsites_fvar %>% dplyr::filter(name_site == "AU-Cpr")
b <- heatscatter(x=df$deficit, y = df$fvar, ggplot = TRUE)
b$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
b <- b +
  labs(
    title = "AU-Cpr",
    y = "fET (-)",
    x = "Cumulative water deficit (mm)") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 17),
    axis.title=element_text(size = 19),
    strip.text = element_text(size=17) # plot title of facet_wrap multiplot
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300))
b

# create combined figure with subpanels
ggarrange(a, b,
          labels = c("a", "b"),
          ncol = 2, nrow = 1,
          common.legend = TRUE, # have just one common legend
          legend="top") # and place it in the bottom

# save figure
ggsave("density_AU.png",
       path = "./",
       width = 10,
       height = 4,
)



#### using normal points with transparency
df <- plot_allsites_fvar %>% dplyr::filter(name_site == "AU-Stp")
b <- heatscatter(x=df$deficit, y = df$fvar, ggplot = TRUE)
b$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 17),
    axis.title=element_text(size = 19),
    strip.text = element_text(size=17) # plot title of facet_wrap multiplot
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300))
b

df <- plot_allsites_fvar %>% dplyr::filter(name_site == "AU-Cpr")
d <- ggplot(df, aes(deficit, fvar)) +
  geom_point(alpha = 1/log(nrow(df)), colour = "blue") +
  labs(
    title = "",
    y = "fET (-)",
    x = "Cumulative water deficit (mm)") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 17),
    axis.title=element_text(size = 19),
    strip.text = element_text(size=17) # plot title of facet_wrap multiplot
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300))
d





# MEDIUM ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "medium fET")

b <- ggplot(df, aes(deficit, fvar)) +
  geom_point(alpha = 1/log(nrow(df))) +
  labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 17),
    axis.title=element_text(size = 19),
    strip.text = element_text(size=17) # plot title of facet_wrap multiplot
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +
  facet_wrap(~name_site, ncol = 4)
b

# save figure
ggsave("facet_mediumET_alpha.png",
       path = "./",
       width = 10,
       height = 12,
)



