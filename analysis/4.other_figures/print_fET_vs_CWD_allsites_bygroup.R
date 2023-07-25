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

## load EVI
# load dataframes used as model input (cleaned data)
file_locations_df <- list.files("data/output",
                                glob2rx("ddf_??????.RData"),
                                full.names = TRUE,
                                recursive = TRUE
)

ddf_allsites <- do.call(
  "rbind",
  lapply(
    file_locations_df,
    function(file){
      load(file)
      print(file)

      ddf <- ddf %>%
        mutate(name_site = substr(basename(file), 5, 10)) %>%
        dplyr::select(date, name_site, EVI)

    }
  )
)

# merge EVI to our df
plot_allsites_fvar <- plot_allsites_fvar %>%
  left_join(ddf_allsites, by = c("date", "name_site"))

# remove outliers and scale EVI
plot_allsites_EVI_raw <- plot_allsites_fvar %>%
  group_by(name_site) %>%
  # # in the end we do not normalize EVI (too noisy, it becomes even more) + you can see the decline even without normalization
  # mutate(
  #   EVI_scaled = EVI / median(EVI[deficit < 20])) %>% # take the median of the lower bin of the deficit
  mutate(
    EVI_scaled = EVI) %>%
  filter(between(EVI_scaled,
                 mean(EVI_scaled, na.rm=TRUE) - (2.5 * sd(EVI_scaled, na.rm=TRUE)), # remove outliers by site
                 mean(EVI_scaled, na.rm=TRUE) + (2.5 * sd(EVI_scaled, na.rm=TRUE)))) %>%
  mutate(bin = as.numeric(cut_number(deficit,{n()/50}))) %>% # dynamic number of bins: equal to
                                                                 # number of rows per PFT divided by 40
                                                                 # aka at least 50 points per bin
  ungroup()

# calculate quartiles and median in every bin
df_percentiles <- plot_allsites_EVI_raw %>%
  group_by(bin, name_site) %>%
  summarise(
    perc_25 = stats::quantile(EVI_scaled, probs=0.25, na.rm = TRUE),
    perc_50 = stats::quantile(EVI_scaled, probs=0.5, na.rm = TRUE),
    perc_75 = stats::quantile(EVI_scaled, probs=0.75, na.rm = TRUE),
    medianCWD = stats::quantile(deficit, probs=0.5, na.rm = TRUE)) %>%
  ungroup()

# merge back
plot_allsites_EVI <- plot_allsites_EVI_raw %>%
  left_join(df_percentiles, by = c("bin", "name_site"))

### HIGH ET ####
# get a list of high ET sites and plot them individually
df <- plot_allsites_EVI %>% dplyr::filter(cluster == "high fET")
high_fET <- unique(df$name_site) # list of sites within fET group
results_highET <- list() # initialize list to save plots

for (i in 1:length(high_fET)){
  site <- high_fET[i]

  df <- plot_allsites_EVI %>% dplyr::filter(name_site == site)
  a <- heatscatter(x=df$deficit, y = df$fvar, ggplot = TRUE)
  a$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
  a <- a + labs(y = "Unitless",
                x = "CWD (mm)",
                title = site) +
    geom_ribbon(data = df, aes(x = medianCWD, ymin = perc_25, ymax = perc_75), fill = "#0CFA07", alpha=0.3) +
    geom_line(data = df, aes(x = medianCWD, y = perc_50), color="#0CFA07", alpha=0.9, size = 1) +
    theme_classic() +
    theme(
      axis.text=element_text(size = 14),
      axis.title=element_text(size = 17),
      plot.title = element_text(hjust = 0.5, size = 19) # center title and change size
    ) +
    scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 325), expand = c(0, 0))
  a

  results_highET[[length(results_highET) + 1]] <- a # store plot in a list
  print(high_fET[i])
}

# functions to remove x and y axes when not needed
{remove_y <- theme(
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
results_highET[[16]] <- results_highET[[16]] + remove_y}

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
df <- plot_allsites_EVI %>% dplyr::filter(cluster == "medium fET")
medium_fET <- unique(df$name_site) # list of sites within fET group
results_mediumET <- list() # initialize list to save plots

for (i in 1:length(medium_fET)){
  site <- medium_fET[i]

  df <- plot_allsites_EVI %>% dplyr::filter(name_site == site)
  a <- heatscatter(x=df$deficit, y = df$fvar, ggplot = TRUE)
  a$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
  a <- a + labs(y = "Unitless",
                x = "CWD (mm)",
                title = site) +
    geom_ribbon(data = df, aes(x = medianCWD, ymin = perc_25, ymax = perc_75), fill = "#0CFA07", alpha=0.3) +
    geom_line(data = df, aes(x = medianCWD, y = perc_50), color="#0CFA07", alpha=0.9, size = 1) +
    theme_classic() +
    theme(
      axis.text=element_text(size = 14),
      axis.title=element_text(size = 17),
      plot.title = element_text(hjust = 0.5, size = 19) # center title and change size
    ) +
    scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 325), expand = c(0, 0))
  a

  results_mediumET[[length(results_mediumET) + 1]] <- a # store plot in a list
  print(medium_fET[i])
}

# functions to remove x and y axes when not needed
{remove_y <- theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank())

  remove_x <- theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank())

  remove_x2 <- theme(
    axis.title.x = element_blank())

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
  results_mediumET[[22]] <- results_mediumET[[22]] + remove_y}

# create one plot
wrap_plots(results_mediumET, ncol = 4, nrow = 6) + plot_layout(guides = "collect")

# save figure
ggsave("facet_mediumET.png",
       path = "./",
       width = 12,  # by increasing ratio here, the points on final figure will appear smaller/bigger (i.e. 10-12 will yield smaller points than 5-6)
       height = 15)


### LOW ET ####
# get a list of low ET sites and plot them individually
df <- plot_allsites_EVI %>% dplyr::filter(cluster == "low fET")
low_fET <- unique(df$name_site) # list of sites within fET group
results_lowET <- list() # initialize list to save plots

for (i in 1:length(low_fET)){
  site <- low_fET[i]

  df <- plot_allsites_EVI %>% dplyr::filter(name_site == site)
  a <- heatscatter(x=df$deficit, y = df$fvar, ggplot = TRUE)
  a$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
  a <- a + labs(y = "Unitless",
                x = "CWD (mm)",
                title = site) +
    geom_ribbon(data = df, aes(x = medianCWD, ymin = perc_25, ymax = perc_75), fill = "#0CFA07", alpha=0.3) +
    geom_line(data = df, aes(x = medianCWD, y = perc_50), color="#0CFA07", alpha=0.9, size = 1) +
    theme_classic() +
    theme(
      axis.text=element_text(size = 14),
      axis.title=element_text(size = 17),
      plot.title = element_text(hjust = 0.5, size = 19) # center title and change size
    ) +
    scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 325), expand = c(0, 0))
  a

  results_lowET[[length(results_lowET) + 1]] <- a # store plot in a list
  print(low_fET[i])
}


# functions to remove x and y axes when not needed
{remove_y <- theme(
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
results_lowET[[11]] <- results_lowET[[11]] + remove_y}

# create one plot
wrap_plots(results_lowET, ncol = 3, nrow = 4) + plot_layout(guides = "collect")

# save figure
ggsave("facet_lowET.png",
       path = "./",
       width = 10,  # by increasing ratio here, the points on final figure will appear smaller/bigger (i.e. 10-12 will yield smaller points than 5-6)
       height = 10)


# legend ------------------------------------------------------------------

# # create legend
# ggplot(plot_allsites_EVI,
#        aes(x = deficit))+
#   geom_point(aes(y = fvar, color = "fET")) +
#   # geom_line(aes(y = EVI, color = "MODIS EVI")) +
#   scale_color_manual(values = c("fET" = "red", "MODIS EVI" = "#0CFA07"))
# # scale_linetype_manual(values=c(1,2,NA))+
# #   scale_shape_manual(values=c(NA,NA,2)) +
#
#
# ggplot()+
#   # geom_point(data=plot_allsites_EVI,aes(y = fvar, x = deficit, colour="fET"),size=1 )+
#   geom_line(data=plot_allsites_EVI,aes(y= EVI,x = deficit, colour="MODIS EVI", ),size=1) +
#   theme_bw() +
#   scale_color_manual(values = c("fET" = "red", "MODIS EVI" = "#0CFA07")) +
#   theme(legend.title=element_blank(),
#         legend.box.background = element_rect(colour = "black"))
#
# ggsave("facet_legend2.png",
#        path = "./",
#        width = 4,  # by increasing ratio here, the points on final figure will appear smaller/bigger (i.e. 10-12 will yield smaller points than 5-6)
#        height = 4,
# )
