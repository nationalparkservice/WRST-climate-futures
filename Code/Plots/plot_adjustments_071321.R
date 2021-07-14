####################################################
##    Plot Adjustments 7/13/21  ####################
####################################################

# Using Water Balance from ACCESS1-3
# Example data is saved under './maps'

rm(list = ls())

library(ggplot2)
library(lemon)

# Load example data

d45 <- readRDS('./maps/delta45')
d85 <- readRDS('./maps/delta85')
shp <- readRDS('./maps/shp')

# ggplot

gg45 <- ggplot() + 
  geom_stars(data = d45, alpha = 0.8) + 
  geom_sf(data = shp, aes(), fill = NA) + 
  scale_fill_viridis(direction=-1, option = "G",begin = .5, end = 1, 
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #mako for WB delta
  labs(title = "ACCESS1-3 RCP 4.5") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(.2, "cm"),
        plot.title=element_text(size=12,face="bold",hjust=0.5)) + 
  labs(fill = "Water Balance")

gg85 <- ggplot() + 
  geom_stars(data = d85, alpha = 0.8) + 
  geom_sf(data = shp, aes(), fill = NA) + 
  scale_fill_viridis(direction=-1, option = "G",begin = .5, end = 1, 
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #mako for WB delta
  labs(title = "ACCESS1-3 RCP 8.5") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(.2, "cm"),
        plot.title=element_text(size=12,face="bold",hjust=0.5))

title <- grid::textGrob("Shared Title")

grid_arrange_shared_legend(gg45, gg85, ncol = 2, nrow = 1, position = "bottom", top = title)

