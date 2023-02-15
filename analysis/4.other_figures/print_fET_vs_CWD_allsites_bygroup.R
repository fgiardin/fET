
# load packages
library(LSD) # load basic functions of LSD and then overwrite with our function
devtools::load_all(".")
library(tidyverse)
library(patchwork)

# load data and function
load("data/dataframes/plot_allsites_fvar.RData")
source("~/fET/R/LSD.heatscatter.R")

# HIGH ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "high fET")
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
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +  #
  facet_wrap(~name_site, ncol = 4)
b

# save figure
ggsave("facet_highET.png",
       path = "./",
       width = 10,  # by increasing ratio here, the points on final figure will appear smaller/bigger (i.e. 10-12 will yield smaller points than 5-6)
       height = 10,
)

# HIGH ET -- New Method

# get a list of high ET sites and plot them individually
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "high fET")
high_fET <- unique(df$name_site) # list of sites within fET group
results_highET <- list() # initialize list to save plots

for (i in 1:length(high_fET)){
  site <- high_fET[i]

  df <- plot_allsites_fvar %>% dplyr::filter(name_site == site)
  a <- heatscatter(x=df$deficit, y = df$fvar, ggplot = TRUE)
  a$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
  a <- a + labs(y = "fET (-)",
                x = "CWD (mm)",
                title = site) +
    theme_classic() +
    theme(
      axis.text=element_text(size = 17),
      axis.title=element_text(size = 19),
      strip.text = element_text(size=17), # plot title of facet_wrap multiplot
      plot.title = element_text(hjust = 0.5, size = 19) # center title and change size
    ) +
    scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
    scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300))

  results_highET[[length(results_highET) + 1]] <- a # store plot in a list
  print(i)
}

# functions to remove x and y axes when not needed
remove_y <- theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank()
)

remove_x <- theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank()
)

results_highET[[1]] <- results_highET[[1]] + remove_x
results_highET[[2]] <- results_highET[[2]] + remove_x + remove_y
results_highET[[3]] <- results_highET[[3]] + remove_x + remove_y
results_highET[[4]] <- results_highET[[4]] + remove_x + remove_y

results_highET[[5]] <- results_highET[[5]] + remove_x
results_highET[[6]] <- results_highET[[6]] + remove_x + remove_y
results_highET[[7]] <- results_highET[[7]] + remove_x + remove_y
results_highET[[8]] <- results_highET[[8]] + remove_x + remove_y

results_highET[[9]] <- results_highET[[9]] + remove_x
results_highET[[10]] <- results_highET[[10]] + remove_x + remove_y
results_highET[[11]] <- results_highET[[11]] + remove_x + remove_y
results_highET[[12]] <- results_highET[[12]] + remove_x + remove_y

results_highET[[13]] <- results_highET[[13]]
results_highET[[14]] <- results_highET[[14]] + remove_y
results_highET[[15]] <- results_highET[[15]] + remove_y
results_highET[[16]] <- results_highET[[16]] + remove_y

# create one plot
wrap_plots(results_highET, ncol = 4, nrow = 4) + plot_layout(guides = "collect")

# save figure
ggsave("facet_highET.png",
       path = "./",
       width = 10,  # by increasing ratio here, the points on final figure will appear smaller/bigger (i.e. 10-12 will yield smaller points than 5-6)
       height = 10,
)



# MEDIUM ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "medium fET")

b <- heatscatter(x=df$deficit, y = df$fvar, pch = "6", ggplot = TRUE)
b$data$name_site = df$name_site
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
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
ggsave("facet_mediumET.png",
       path = "./",
       width = 10,
       height = 12,
)


# LOW ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "low fET")

b <- heatscatter(x=df$deficit, y = df$fvar, pch = "6", ggplot = TRUE)
b$data$name_site = df$name_site
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 17),
    axis.title=element_text(size = 19),
    strip.text = element_text(size=17) # plot title of facet_wrap multiplot
  ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +  #
  facet_wrap(~name_site, ncol = 3)
b

# save figure
ggsave("facet_lowET.png",
       path = "./",
       width = 8,
       height = 8,
)

