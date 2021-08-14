var = "tminF"
long.title = "minimum temperature (\u00B0F)" 
delta.var = "tmin"
scale="inferno"

shp = wrst # area is wrst

# create CF file lists
rds.ls = list.files(path = dir, pattern = paste0(var,"_"), full.names = TRUE)
CF1.ls = Filter(function(x) grepl(paste(GCMs[1], collapse = "|"), x), rds.ls)
CF2.ls = Filter(function(x) grepl(paste(GCMs[2], collapse = "|"), x), rds.ls)
CF3.ls = Filter(function(x) grepl(paste(GCMs[3], collapse = "|"), x), rds.ls)

# ggplot
map.plot <- function(data, title,xaxis,metric,col){
  ggplot() + 
    geom_stars(data = data, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    scale_fill_viridis(direction=1, option = scale, 
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #mako for WB delta
    labs(title = title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          # plot.title=element_blank(),
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, size=5),
          plot.margin = unit(c(.5,0,0,0), "cm")) + 
    labs(fill = metric)
}

# CF1
DJF.cf1 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("DJF", collapse = "|"), x), CF1.ls)),
                    title=CFs[1],metric=paste0("Average ",long.title),col=CF_GCM$CF_col[1])
MAM.cf1 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("MAM", collapse = "|"), x), CF1.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[1])
JJA.cf1 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("JJA", collapse = "|"), x), CF1.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[1])
SON.cf1 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("SON", collapse = "|"), x), CF1.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[1])

# CF2
DJF.cf2 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("DJF", collapse = "|"), x), CF2.ls)),
                    title=CFs[2],metric=paste0("Average ",long.title),col=CF_GCM$CF_col[2])
MAM.cf2 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("MAM", collapse = "|"), x), CF2.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[2])
JJA.cf2 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("JJA", collapse = "|"), x), CF2.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[2])
SON.cf2 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("SON", collapse = "|"), x), CF2.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[2])

# CF3
DJF.cf3 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("DJF", collapse = "|"), x), CF3.ls)),
                    title=CFs[3],metric=paste0("Average ",long.title),col=CF_GCM$CF_col[3])
MAM.cf3 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("MAM", collapse = "|"), x), CF3.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[3])
JJA.cf3 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("JJA", collapse = "|"), x), CF3.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[3])
SON.cf3 <- map.plot(data=readRDS(Filter(function(x) grepl(paste("SON", collapse = "|"), x), CF3.ls)),
                    title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[3])

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

djf <- textGrob("DJF", gp = gpar(fontface="bold", size = 12))
mam <- textGrob("MAM", gp = gpar(fontface="bold", size = 12))
jja <- textGrob("JJA", gp = gpar(fontface="bold", size = 12))
son <- textGrob("SON", gp = gpar(fontface="bold", size = 12))

seasons <- grid.arrange(djf,mam,jja,son,ncol=1)

maps <- grid.arrange(seasons,maps.all,ncol = 2, widths = c(1,15))


################################### MONTHLY DOT PLOT ##################


dotplot <- ggplot(delta, aes(x=(eval(parse(text=delta.var))),y=season,fill=CF)) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") + 
  geom_point(stat="identity",size=8,colour="black",aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),    #Text size for axis tick mark labels
        axis.title.x=element_blank(),               #Text size and alignment for x-axis label
        plot.title=element_blank(),
        # axis.title.y=element_text(size=16, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
        # plot.title=element_text(size=20, vjust=0.5, face="bold", margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
        legend.title=element_text(size=16),                                                                    #Text size of legend category labels
        legend.text=element_text(size=14),                                                                   #Text size of legend title
        legend.position = "bottom")  +
  labs(title = paste0("Change in seasonal ",long.title), 
       x = "Change (°F)", y = "") +
  scale_fill_manual(name="",values =cols) +
  scale_shape_manual(name="",values = c(21,22,23)) +
  scale_y_discrete(limits=rev)
dotplot

g <- grid.arrange(maps, dotplot,ncol = 2, widths = c(6, 4), clip = FALSE)

annotate_figure(g, top = text_grob(paste0("Change in seasonal ",long.title, "; 1950-1999 vs 2025-2055"), 
                                      face = "bold", size = 20))

ggsave(paste0("seasonal_",var,".png"), width = 15, height = 9, path = plot.dir)
