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

###########################################################
##### NONE OF THESE WORK ##################################
# Merge into one plot

maps.all <- grid_arrange_shared_legend(DJF.cf1,DJF.cf2,DJF.cf3, # THIS WORKS, BUT WANT TO HAVE SHARED RC TITLES
                                    MAM.cf1,MAM.cf2,MAM.cf3,    # FOR CF AND SEASON
                                    JJA.cf1,JJA.cf2,JJA.cf3,
                                    SON.cf1,SON.cf2,SON.cf3,
                                    nrow = 4,ncol=3, position = "bottom") 
                                    # top = textGrob("Change in average annual water balance (in/yr)",
                                    #                gp=gpar(fontface="bold", col="black", fontsize=20)))

DJFcf1.grob <- ggplotGrob(DJF.cf1)

lg <- tableGrob(c("W", "S","S","F"), theme= ttheme_minimal())
grobS=c(grob(DJF.cf1),grob(DJF.cf2),grob(DJF.cf3))
combine <- rbind(tableGrob(t(c(letters[1:3])), theme = ttheme_minimal(), rows = ""), 
                 cbind(tableGrob(LETTERS[1:4], theme = ttheme_minimal()), 
                       arrangeGrob(grobs = maps.all),  size = "last"), size = "last")
grid.newpage()
grid.draw(cbind(lg, maps.all, size = "last"))


################################### MONTHLY DOT PLOT ##################

sample$month <- factor(sample$month, levels=c("Dec","Jan","Feb","Mar","Apr","May","Jun","Jul",
                                                 "Aug","Sep","Oct","Nov"))
dotplot <- ggplot(sample, aes(x=water.balance,y=month,fill=CF)) +
  geom_point(stat="identity",size=8,colour="black",aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),    #Text size for axis tick mark labels
        axis.title.x=element_blank(),               #Text size and alignment for x-axis label
        plot.title=element_blank(),
        # axis.title.y=element_text(size=16, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
        # plot.title=element_text(size=20, vjust=0.5, face="bold", margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
        legend.title=element_text(size=16),                                                                    #Text size of legend category labels
        legend.text=element_text(size=14),                                                                   #Text size of legend title
        legend.position = "bottom")  +
  labs(title = "Change in average monthly water balance (in/yr)", 
       x = "Change in water balance (in/yr)", y = "") +
  scale_fill_manual(name="",values = c("blue", "pink", "red")) +
  scale_shape_manual(name="",values = c(21,22,23)) +
  scale_y_discrete(limits=rev)
dotplot

g <- ggarrange(maps.all,dotplot, nrow=1,ncol=2)
g
annotate_figure(g, top = text_grob("Change in average monthly water balance (in/month)", 
                                      face = "bold", size = 20))












#################################### CODE TO TRY AND ARRANGE GRID
# https://stackoverflow.com/questions/45473843/put-row-and-column-titles-using-grid-arrange-in-r
# doesn't work b/c each map returns 9 grobs ?
rm(list=ls())

pl <- replicate(12, ggplot(), FALSE)

N <- length(pl)
nr <- 4
nc <- 3


combine <- rbind(tableGrob(t(c(letters[1:nc])), theme = ttheme_minimal(), rows = ""), 
                 cbind(tableGrob(LETTERS[1:nr], theme = ttheme_minimal()), 
                       arrangeGrob(grobs = pl),  size = "last"), size = "last")
grid.newpage()
grid.draw(combine)



#https://stackoverflow.com/questions/60347583/add-row-and-column-titles-with-ggarrange
# Promising but 1. scale bar in weird location, 2. horizontal alignment weird

maps.all <- grid_arrange_shared_legend(DJF.cf1,DJF.cf2,DJF.cf3, # THIS WORKS, BUT WANT TO HAVE SHARED RC TITLES
                                       MAM.cf1,MAM.cf2,MAM.cf3,    # FOR CF AND SEASON
                                       JJA.cf1,JJA.cf2,JJA.cf3,
                                       SON.cf1,SON.cf2,SON.cf3,
                                       nrow = 4,ncol=3, position = "bottom") 


row1 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="row 1 title", angle = 90) + theme_void() 
row2 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="row 2 title", angle = 90) + theme_void() 
row3 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="row 3 title", angle = 90) + theme_void() 
row4 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="row 4 title", angle = 90) + theme_void() 

col1 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="col 1 title") + theme_void() 
col2 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="col 2 title") + theme_void() 
col3 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="col 3 title") + theme_void()

layoutplot <- "
#cccddd
aeeefff
aeeefff
bggghhh
bggghhh
"


plotlist <- list(a = row1, b = row2, c = col1, d = col2, e= DJF.cf1, f=DJF.cf2, g=MAM.cf1, h=MAM.cf2)

wrap_plots(plotlist, guides = 'collect', design = layoutplot)




#https://wilkelab.org/cowplot/articles/plot_grid.html

# first align the top-row plot (p3) with the left-most plot of the
# bottom row (p1)
plots <- align_plots(p3, p1, align = 'v', axis = 'l')
# then build the bottom row
bottom_row <- plot_grid(plots[[2]], p2, labels = c('B', 'C'), label_size = 12)

# then combine with the top row for final plot
plot_grid(plots[[1]], bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)


# https://stackoverflow.com/questions/45473843/put-row-and-column-titles-using-grid-arrange-in-r
# Again, too many grobs for each of the maps

set.seed(0)
pl = lapply(1:9, function(i) {
  p = ggplot(data.frame(x=1:10,y=rnorm(10)),aes(x, y)) + 
    geom_line()
})


maps.all <- grid_arrange_shared_legend(DJF.cf1,DJF.cf2,DJF.cf3, # THIS WORKS, BUT WANT TO HAVE SHARED RC TITLES
                                       MAM.cf1,MAM.cf2,MAM.cf3,    # FOR CF AND SEASON
                                       JJA.cf1,JJA.cf2,JJA.cf3,
                                       SON.cf1,SON.cf2,SON.cf3,
                                       nrow = 4,ncol=3, position = "bottom") 

# Create row and column titles
col.titles = paste("CF", 1:3)
row.titles = paste("R_Title", 4:6)

# Add row titles
pl[1:3] = lapply(1:3, function(i) arrangeGrob(pl[[i]], left=row.titles[i]))

DJF.cf1 <- arrangeGrob(DJF.cf1,left=row.titles[1])
MAM.cf1 <- arrangeGrob(MAM.cf1,left=row.titles[2])
MAM.cf1 <- arrangeGrob(JJA.cf1,left=row.titles[3])

# Add column titles and lay out plots
grid.arrange(grobs=c(arrangeGrob(DJF.cf1,top=col.titles[1],ncol=1),
                     arrangeGrob(DJF.cf2,top=col.titles[2],ncol=1),
                     arrangeGrob(DJF.cf2,top=col.titles[3],ncol=1)),ncol=3)


               