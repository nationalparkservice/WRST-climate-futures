rm(list = ls())

library(ggplot2)
library(lemon)
library(stars)
library(viridis)
library(ggthemes)
library(ggpubr)
library(gridExtra)
library(grid)
library(dplyr)

# Load example data

d45 <- readRDS('./maps/delta45')
d85 <- readRDS('./maps/delta85')
shp <- readRDS('./maps/shp')

# Generate sample data for ts plot


sample = data.frame(month=format(seq(as.Date("2000/1/1"), by = "month", length.out = 12), "%b")
                    ,CF = "CF1",water.balance=rnorm(12,1,1.5))
cf2 <- data.frame(month=format(seq(as.Date("2000/1/1"), by = "month", length.out = 12), "%b")
                  ,CF = "CF2",water.balance=rnorm(12,0,3.5))
cf3 <- data.frame(month=format(seq(as.Date("2000/1/1"), by = "month", length.out = 12), "%b")
                  ,CF = "CF3",water.balance=rnorm(12,-2,1))
sample <- rbind(sample,cf2,cf3);rm(cf2,cf3)
sample$CF <- as.factor(sample$CF)

# ggplot
map.plot <- function(data, title,xaxis,metric,col){
  ggplot() + 
    geom_stars(data = data, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    scale_fill_viridis(direction=-1, option = "G",begin = .5, end = 1, 
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #mako for WB delta
    labs(title = title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          plot.title=element_blank(),
          # plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, size=5)) + 
    labs(fill = metric)
}

# CF1
DJF.cf1 <- map.plot(data=d45,title="ACCESS1-3 RCP 4.5",metric="Water Balance",col="blue")
MAM.cf1 <- map.plot(data=d45,title="",metric="Water Balance",col="blue")
JJA.cf1 <- map.plot(data=d45,title="",metric="Water Balance",col="blue")
SON.cf1 <- map.plot(data=d45,title="",metric="Water Balance",col="blue")

# CF2
DJF.cf2 <- map.plot(data=d45,title="ACCESS1-3 RCP 8.5",metric="Water Balance",col="pink")
MAM.cf2 <- map.plot(data=d45,title="",metric="Water Balance",col="pink")
JJA.cf2 <- map.plot(data=d45,title="",metric="Water Balance",col="pink")
SON.cf2 <- map.plot(data=d45,title="",metric="Water Balance",col="pink")

# CF3
DJF.cf3 <- map.plot(data=d45,title="Climate Future 3",metric="Water Balance",col="red")
MAM.cf3 <- map.plot(data=d45,title="",metric="Water Balance",col="red")
JJA.cf3 <- map.plot(data=d45,title="",metric="Water Balance",col="red")
SON.cf3 <- map.plot(data=d45,title="",metric="Water Balance",col="red")

# Merge into one plot
lg <- tableGrob(c("W", "S","S","F"), theme= ttheme_minimal())
maps.all <- grid_arrange_shared_legend(DJF.cf1,DJF.cf2,DJF.cf3,
                                    MAM.cf1,MAM.cf2,MAM.cf3,
                                    JJA.cf1,JJA.cf2,JJA.cf3,
                                    SON.cf1,SON.cf2,SON.cf3,
                                    nrow = 4,ncol=3, position = "bottom", 
                                    top = textGrob("Change in average annual water balance (in/yr)",
                                                   gp=gpar(fontface="bold", col="black", fontsize=16)))

grobS=c(grob(DJF.cf1),grob(DJF.cf2),grob(DJF.cf3))
combine <- rbind(tableGrob(t(c(letters[1:3])), theme = ttheme_minimal(), rows = ""), 
                 cbind(tableGrob(LETTERS[1:4], theme = ttheme_minimal()), 
                       arrangeGrob(grobs = maps.all),  size = "last"), size = "last")
grid.newpage()
grid.draw(cbind(lg, maps.all, size = "last"))



