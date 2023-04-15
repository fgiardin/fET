# plot EVI distribution used to train ET-NN vs distribution to train PET-NN

# load libraries
library(tidyverse)
library(ggpubr)
library(patchwork)

# load final list of sites
load("data/dataframes/vec_sites.RData")

# load model input for all sites
file_locations_df <- list.files("data/output",
                                glob2rx("ddf_??????.RData"),
                                full.names = TRUE,
                                recursive = TRUE
                                )

# extract netrad and temperature
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

# load model output for all sites
file_locations_out <- list.files("data/output",
                                 glob2rx("out_*.RData"),
                                 full.names = TRUE,
                                 recursive = TRUE
                                 )

# this time we just need the output of the model
out_allsites <- do.call(
  "rbind",
  lapply(
    file_locations_out,
    function(file){
      load(file)
      print(file)

      out$df_all <- out$df_all %>%
        mutate(name_site = substr(basename(file), 5, 10))

    }
  )
)

# merge
merged <- out_allsites %>%
  left_join(ddf_allsites, by = c("date", "name_site")) %>%
  dplyr::filter(name_site %in% vec_sites)


# range and distributions EVI ------------------------------------------------------------

# PET
boxplot(merged %>% dplyr::filter(moist) %>% pull(EVI))
hist(merged %>% dplyr::filter(moist) %>% pull(EVI))

# ET (all data)
boxplot(merged$EVI)
hist(merged$EVI)



# boxplots all sites ------------------------------------------------------
a <- merged %>%
  ggplot(aes(x = moist, y = EVI)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(fill="#B2D0AA") +
  theme_classic() +
  geom_hline(aes(yintercept = 0.8), linetype = "dotted") +
  geom_hline(aes(yintercept = 0.4), linetype = "dotted") +
  labs(y = "EVI (-)") +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
    plot.title = element_text(hjust = 0.5, size=14) # center title,
  ) +
  scale_x_discrete(labels=c('Dry days', 'Moist days'))
plot(a)


# histogram all sites ---------------------------------------------------------------
merged$moist <- factor(merged$moist, levels = c("TRUE", "FALSE"))
a <- merged %>%
  ggplot(aes(x = moist, y = EVI, fill = moist)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() + #fill="#B2D0AA"
  theme_classic() +
  geom_hline(aes(yintercept = 0.8), linetype = "dotted") +
  geom_hline(aes(yintercept = 0.4), linetype = "dotted") +
  labs(y = "EVI (-)") +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
    legend.position="none"
  ) +
  scale_x_discrete(labels=c('Moist days', 'Dry days')) +
  scale_fill_manual(labels = c("Moist days", "Dry days"), values = c("#9297FF", "#FF9594"))
plot(a)

b <- ggplot(merged ,
            aes(x = EVI,
                fill = moist)) +
  geom_histogram(alpha=0.5, position="identity", color ="grey30") +
  theme_classic() +
  theme(
    axis.text=element_text(size = 12),
    axis.title=element_text(size = 14),
    legend.text=element_text(size=12),
    legend.position=c(.75,.9),
    legend.title=element_blank()
  ) +
  xlab("EVI (-)") + # nested expression() with two "paste()" inside, one for the entire subscript and the other for the units
  ylab("Frequency") +
  scale_fill_manual(labels = c("Moist days", "Dry days"), values = c("blue", "red"))
b


# boxplots site by site ------------------------------------------------------

# ET (all data)
c <- merged %>%
  ggplot(aes(x = name_site, y = EVI)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(fill="#B2D0AA") +
  theme_classic() +
  geom_hline(aes(yintercept = 0.8), linetype = "dotted") +
  geom_hline(aes(yintercept = 0.4), linetype = "dotted") +
  labs(y = "EVI (-)",
       title = expression(paste("All data - ", DNN[ET]))
       ) +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text=element_text(size=10),
    axis.title=element_text(size=14),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # arrange x axis labels to be readable
    plot.title = element_text(hjust = 0.5, size=14) # center title,

  )
plot(c)

# PET (moist data)
d <- merged %>%
  dplyr::filter(moist) %>%
  ggplot(aes(x = name_site, y = EVI)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(fill="grey90") +
  theme_classic() +
  geom_hline(aes(yintercept = 0.8), linetype = "dotted") +
  geom_hline(aes(yintercept = 0.4), linetype = "dotted") +
  labs(y = "EVI (-)",
       title = expression(paste("Moist data - ", DNN[PET]))
  ) +
  theme(
    axis.title.x=element_blank(), # remove text of x axis
    axis.text=element_text(size=10),
    axis.title=element_text(size=14),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), # arrange x axis labels to be readable
    plot.title = element_text(hjust = 0.5, size=14) # center title,

  )
plot(d)

# plot together using patchwork
(a | b) / c / d +
  plot_annotation(tag_levels ="a") &
  theme(plot.tag = element_text(face="bold"))


# save plot
ggsave("EVI_range.png", path = "./", width = 10, height = 15)
