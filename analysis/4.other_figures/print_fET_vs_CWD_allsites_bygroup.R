library(LSD) # load basic functions of LSD and then overwrite with beni's function
devtools::load_all(".")
library(tidyverse)  

# load data: /Users/fgiardina/Desktop/fET vs CWD allsites facet/dataframes
load("/Users/fgiardina/fvar/output/plot_allsites_fvar.RData")

# HIGH ET 
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "high fET")
b <- heatscatter(x=df$deficit, y = df$fvar, cexplot = 0.5, ggplot = TRUE) 
b$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  # theme(
  #   axis.text=element_text(size = 14),
  #   axis.title=element_text(size = 16),
  #   legend.text=element_text(size=14)
  # ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +  #
  facet_wrap(~name_site, ncol = 4)
b

ggsave("facet_highET.png", 
       path = "./output", 
       width = 5,
       height = 5,
)

# 
# ## Ankit's fix
# get_density <- function(x, y, ...) {
#   dens <- MASS::kde2d(x, y, ...)
#   ix <- findInterval(x, dens$x)
#   iy <- findInterval(y, dens$y)
#   ii <- cbind(ix, iy)
#   return(dens$z[ii])
# }
# 
# df %>% dplyr::mutate(density = get_density(deficit, fvar, h = c(1,1), n = 300)) %>% 
#   ggplot(., aes(x = deficit, y = fvar, fill = density, color = density)) +
#   geom_point(shape = 21) + 
#   scale_fill_distiller(palette = 'YlGnBu', direction = 1) +
#   scale_color_distiller(palette = 'YlGnBu', direction = 1) + 
#   theme_classic() +
#   scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
#   scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +
#   facet_wrap(~name_site, ncol = 4)


# MEDIUM ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "medium fET")

b <- heatscatter(x=df$deficit, y = df$fvar, pch = "6", ggplot = TRUE) 
b$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  # theme(
  #   axis.text=element_text(size = 14),
  #   axis.title=element_text(size = 16),
  #   legend.text=element_text(size=14)
  # ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +  #
  facet_wrap(~name_site, ncol = 4)
b

ggsave("facet_mediumET.png", 
       path = "./output", 
       width = 5,
       height = 6,
       )

# LOW ET
df <- plot_allsites_fvar %>% dplyr::filter(cluster == "low fET")

b <- heatscatter(x=df$deficit, y = df$fvar, pch = "6", ggplot = TRUE) 
b$data$name_site = df$name_site   # hack the name of the site back into the ggplot object (it won't take it from original df through heatscatter function)
b <- b + labs(y = "fET (-)", x = "Cumulative water deficit (mm)") +
  theme_classic() +
  # theme(
  #   axis.text=element_text(size = 14),
  #   axis.title=element_text(size = 16),
  #   legend.text=element_text(size=14)
  # ) +
  scale_y_continuous(breaks = seq(0, 1.4, 0.4), limits = c(0, 1.4)) +
  scale_x_continuous(breaks = seq(0, 300, 100), limits = c(0, 300)) +  #
  facet_wrap(~name_site, ncol = 4)
b

ggsave("facet_lowET.png", 
       path = "./output", 
       width = 5,
       height = 3,
)

