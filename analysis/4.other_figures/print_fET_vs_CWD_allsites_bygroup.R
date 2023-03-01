# Script to print fET vs CWD site-by-site (1 plot per fET group)
# originally using facet_wrap but the density function didn't work properly

# load packages
library(LSD) # load basic functions of LSD and then overwrite with our function
devtools::load_all(".")
library(tidyverse)
library(patchwork)

# load data and function
load("data/dataframes/plot_allsites_fvar.RData")
source("~/fET/R/LSD.heatscatter.R")

### HIGH ET ####
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
      strip.text = element_text(size=17), # plot title
      plot.title = element_text(hjust = 0.5, size = 19) # center title and change size
    ) +
    scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 310), expand = c(0, 0))

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
       width = 12,  # by increasing ratio here, the points on final figure will appear smaller/bigger (i.e. 10-12 will yield smaller points than 5-6)
       height = 10,
)



### MEDIUM ET ####
# get a list of medium ET sites and plot them individually
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "medium fET")
medium_fET <- unique(df$name_site) # list of sites within fET group
results_mediumET <- list() # initialize list to save plots

for (i in 1:length(medium_fET)){
  site <- medium_fET[i]

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
      strip.text = element_text(size=17), # plot title
      plot.title = element_text(hjust = 0.5, size = 19) # center title and change size
    ) +
    scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 310), expand = c(0, 0))

  results_mediumET[[length(results_mediumET) + 1]] <- a # store plot in a list
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

remove_x2 <- theme(
  axis.title.x = element_blank()
)

results_mediumET[[1]] <- results_mediumET[[1]] + remove_x
results_mediumET[[2]] <- results_mediumET[[2]] + remove_x + remove_y
results_mediumET[[3]] <- results_mediumET[[3]] + remove_x + remove_y
results_mediumET[[4]] <- results_mediumET[[4]] + remove_x + remove_y

results_mediumET[[5]] <- results_mediumET[[5]] + remove_x
results_mediumET[[6]] <- results_mediumET[[6]] + remove_x + remove_y
results_mediumET[[7]] <- results_mediumET[[7]] + remove_x + remove_y
results_mediumET[[8]] <- results_mediumET[[8]] + remove_x + remove_y

results_mediumET[[9]] <- results_mediumET[[9]] + remove_x
results_mediumET[[10]] <- results_mediumET[[10]] + remove_x + remove_y
results_mediumET[[11]] <- results_mediumET[[11]] + remove_x + remove_y
results_mediumET[[12]] <- results_mediumET[[12]] + remove_x + remove_y

results_mediumET[[13]] <- results_mediumET[[13]] + remove_x
results_mediumET[[14]] <- results_mediumET[[14]] + remove_x + remove_y
results_mediumET[[15]] <- results_mediumET[[15]] + remove_x + remove_y
results_mediumET[[16]] <- results_mediumET[[16]] + remove_x + remove_y

results_mediumET[[17]] <- results_mediumET[[17]] + remove_x
results_mediumET[[18]] <- results_mediumET[[18]] + remove_x + remove_y
results_mediumET[[19]] <- results_mediumET[[19]] + remove_y
results_mediumET[[20]] <- results_mediumET[[20]] + remove_y

results_mediumET[[21]] <- results_mediumET[[21]]
results_mediumET[[22]] <- results_mediumET[[22]] + remove_y

# create one plot
wrap_plots(results_mediumET, ncol = 4, nrow = 6) + plot_layout(guides = "collect")

# save figure
ggsave("facet_mediumET.png",
       path = "./",
       width = 12,  # by increasing ratio here, the points on final figure will appear smaller/bigger (i.e. 10-12 will yield smaller points than 5-6)
       height = 15,
)


### LOW ET ####
# get a list of low ET sites and plot them individually
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "low fET")
low_fET <- unique(df$name_site) # list of sites within fET group
results_lowET <- list() # initialize list to save plots

for (i in 1:length(low_fET)){
  site <- low_fET[i]

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
      strip.text = element_text(size=17), # plot title
      plot.title = element_text(hjust = 0.5, size = 19) # center title and change size
    ) +
    scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 310), expand = c(0, 0))

  results_lowET[[length(results_lowET) + 1]] <- a # store plot in a list
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

results_lowET[[1]] <- results_lowET[[1]] + remove_x
results_lowET[[2]] <- results_lowET[[2]] + remove_x + remove_y
results_lowET[[3]] <- results_lowET[[3]] + remove_x + remove_y

results_lowET[[4]] <- results_lowET[[4]] + remove_x
results_lowET[[5]] <- results_lowET[[5]] + remove_x + remove_y
results_lowET[[6]] <- results_lowET[[6]] + remove_x + remove_y

results_lowET[[7]] <- results_lowET[[7]] + remove_x
results_lowET[[8]] <- results_lowET[[8]] + remove_x + remove_y
results_lowET[[9]] <- results_lowET[[9]] + remove_y

results_lowET[[10]] <- results_lowET[[10]]
results_lowET[[11]] <- results_lowET[[11]] + remove_y


# create one plot
wrap_plots(results_lowET, ncol = 3, nrow = 4) + plot_layout(guides = "collect")

# save figure
ggsave("facet_lowET.png",
       path = "./",
       width = 10,  # by increasing ratio here, the points on final figure will appear smaller/bigger (i.e. 10-12 will yield smaller points than 5-6)
       height = 10,
)


