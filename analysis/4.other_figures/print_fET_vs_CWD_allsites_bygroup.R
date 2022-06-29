library(LSD) # load basic functions of LSD and then overwrite with our function
devtools::load_all(".")
library(tidyverse)

# load data and function
load("manuscript/Figures/dataframes/plot_allsites_fvar.RData")
source("~/fET/R/LSD.heatscatter.R")

# HIGH ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "high fET")
b <- heatscatter(x=df$deficit, y = df$fvar, ggplot = TRUE)
b$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +  #
  facet_wrap(~name_site, ncol = 4)
b

# save figure
ggsave("facet_highET.png",
       path = "./output",
       width = 5,
       height = 5,
)


# MEDIUM ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "medium fET")

b <- heatscatter(x=df$deficit, y = df$fvar, pch = "6", ggplot = TRUE)
b$data$name_site = df$name_site
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +
  facet_wrap(~name_site, ncol = 4)
b

# save figure
ggsave("facet_mediumET.png",
       path = "./output",
       width = 5,
       height = 6,
)


# LOW ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "low fET")

b <- heatscatter(x=df$deficit, y = df$fvar, pch = "6", ggplot = TRUE)
b$data$name_site = df$name_site
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +  #
  facet_wrap(~name_site, ncol = 4)
b

# save figure
ggsave("facet_lowET.png",
       path = "./output",
       width = 5,
       height = 3,
)

