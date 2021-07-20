####################################################
##    Plot Adjustments 7/13/21  ####################
####################################################

# Using Water Balance from ACCESS1-3
# Example data is saved under './maps'
# Next steps -- create function for plots

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
library(gtable)

# Load example data

d45 <- readRDS('./maps/delta45')
d85 <- readRDS('./maps/delta85')
shp <- readRDS('./maps/shp')

# Generate sample data for ts plot
sample = data.frame(year=seq(1979,2015,1),CF = "Historical",water.balance=rnorm(37,-1,1.5))
cf1 <- data.frame(year=seq(2025,2055,1),CF = "CF1",water.balance=rnorm(31,.5,2))
cf2 <- data.frame(year=seq(2025,2055,1),CF = "CF2",water.balance=rnorm(31,0,3.5))
cf3 <- data.frame(year=seq(2025,2055,1),CF = "CF3",water.balance=rnorm(31,-2.5,1.5))
sample <- rbind(sample,cf1,cf2,cf3);rm(cf1,cf2,cf3)
sample$date <- as.Date(as.character(sample$year),format="%Y")
sample$CF <- as.factor(sample$CF)
means <- sample %>% group_by(CF) %>%
  summarize(water.balance = mean(water.balance)) 


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
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, size=5)) + 
    labs(fill = metric)
}

cf1 <- map.plot(data=d45,title="ACCESS1-3 RCP 4.5",metric="Water Balance",col="blue")

cf2 <- map.plot(data=d85,title="ACCESS1-3 RCP 8.5",metric="Water Balance",col="pink")

cf3 <- map.plot(data=d85,title="Climate Future 3",metric="Water Balance",col="red")

ts <- ggplot(sample, aes(x=year, y=water.balance, group=CF, colour = CF)) +
  # geom_rect(xmin=as.Date("2025",format="%Y"), xmax=as.Date("2055",format="%Y"),
  #           ymin=0, ymax=Inf,fill="grey",alpha=0.2,colour="black") +
  geom_line(colour = "black",size=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        # axis.text.x=element_blank(),
        axis.title.x=element_text(size=16,vjust=1.0),
        axis.title.y=element_text(size=16,vjust=1.0),
        plot.title=element_blank(),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.position = "bottom") +
  labs(title = "Average annual water balance", 
       x = "Year", y = "Water balance (in/yr)") +
  scale_color_manual(name="",values = c("blue","pink","red","grey")) +
  scale_fill_manual(name="",values = c("blue","pink","red","grey")) +
  scale_shape_manual(name="",values = c(21,22,23, 24)) +
  coord_fixed(ratio = 1.25) 
  # scale_y_continuous(limits=c(ceiling(42), ceiling(max(ALL2$TavgCustom)))) +
  # guides(color=guide_legend(override.aes = list(size=7))) 
ts


bar <- ggplot(means, aes(x=CF,y=water.balance,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  theme(axis.text=element_text(size=14),
        # axis.text.x=element_blank(),
        axis.title=element_blank(),
        plot.title=element_blank(),
        legend.position = "none")  +
  # coord_cartesian(ylim=c(0, 40)) +
  scale_fill_manual(name="",values = c("blue","pink","red","grey"))
bar


#### Just maps and ts plot
maps <- grid_arrange_shared_legend(cf1, cf2, cf3, ncol = 3, nrow = 1, position = "bottom", 
                                   top = textGrob("Change in average annual water balance (in/yr)",
                                                  gp=gpar(fontface="bold", col="black", fontsize=16)))
g <- ggarrange(maps,ts, nrow=2)
g

#### Maps, ts, bar

tsplots <- ggarrange(ts, bar,ncol=2,nrow=1,widths=c(2,1))

g <- ggarrange(maps,tsplots, nrow=2)
g


#### Maps, ts, table
delta.var <- means
delta.var$water.balance[1:3] <- delta.var$water.balance[1:3] - delta.var$water.balance[4]
delta.var$water.balance <- signif(delta.var$water.balance, digits = 1)

table <- tableGrob(delta.var, rows = NULL,cols=NULL)
table <- gtable_add_grob(table, grobs = rectGrob(gp = gpar(fill=NA, lwd=2)), #library(gtable)
                     t=4,b=nrow(table),l=1,r=ncol(table))
table <- annotate_figure(table,
                top = text_grob("Historical = absolute value \n CFs = change values", color = "black",
                                 face = "italic", size = 12))
tsplots <- grid.arrange(ts, table,ncol = 2, widths = c(4, 1), clip = FALSE)

g <- ggarrange(maps,tsplots, nrow=2)
g



