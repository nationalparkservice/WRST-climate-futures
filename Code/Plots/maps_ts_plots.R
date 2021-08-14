var = "Annual.precipIn"
long.title = "total precipitation (in/year)" 
delta.var = "pcp"
scale="viridis"

shp = wrst # area is wrst

# create CF file lists
rds.ls = list.files(path = dir, pattern = paste0(var,"_"), full.names = TRUE)
CF1.ls = Filter(function(x) grepl(paste(GCMs[1], collapse = "|"), x), rds.ls)
CF2.ls = Filter(function(x) grepl(paste(GCMs[2], collapse = "|"), x), rds.ls)
CF3.ls = Filter(function(x) grepl(paste(GCMs[3], collapse = "|"), x), rds.ls)

# Generate sample data for ts plot
df = read.csv(Filter(function(x) grepl(paste(".csv", collapse = "|"), x), rds.ls))
df = merge(df, CF_GCM,by="GCM",all=TRUE)
df$CF[which(is.na((df$CF)))] = "Historical"
df$CF_col[which(is.na((df$CF_col)))] = "grey"
df$CF = factor(df$CF, levels=c(CFs,"Historical"))

means <- df %>% group_by(CF) %>%
  summarize(var = mean(eval(parse(text=var)))) 


# ggplot
map.plot <- function(data, title,xaxis,metric,col){
  ggplot() + 
    geom_stars(data = data, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    scale_fill_viridis(direction=-1, option = scale,
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

cf1 <- map.plot(data=readRDS(CF1.ls),title=CFs[1],metric=long.title,col=cols[1])
cf2 <- map.plot(data=readRDS(CF2.ls),title=CFs[2],metric=long.title,col=cols[2])
cf3 <- map.plot(data=readRDS(CF3.ls),title=CFs[3],metric=long.title,col=cols[3])

ts <- ggplot(df, aes(x=Year, y=(eval(parse(text=var))), group=CF, colour = CF)) +
 
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
  labs(title = paste0("Change in annual ",long.title), 
       x = "Year", y = long.title) +
  scale_color_manual(name="",values = c(cols,"grey")) +
  scale_fill_manual(name="",values = c(cols,"grey")) +
  scale_shape_manual(name="",values = c(21,22,23, 24)) +
  coord_fixed(ratio = .3) 
ts


#### Just maps and ts plot
maps <- grid_arrange_shared_legend(cf1, cf2, cf3, ncol = 3, nrow = 1, position = "bottom", 
                                   top = textGrob(paste0("Change in ",long.title),
                                                  gp=gpar(fontface="bold", col="black", fontsize=16)))
# g <- ggarrange(maps,ts, nrow=2)
# g

#### Maps, ts, table
delta.var <- means
delta.var$var[1:3] <- delta.var$var[1:3] - delta.var$var[4]
delta.var$var <- signif(delta.var$var, digits = 1)

table <- tableGrob(delta.var, rows = NULL,cols=NULL)
table <- gtable_add_grob(table, grobs = rectGrob(gp = gpar(fill=NA, lwd=2)), #library(gtable)
                     t=4,b=nrow(table),l=1,r=ncol(table))
table <- annotate_figure(table,
                top = text_grob("Historical = absolute value \n CFs = change values", color = "black",
                                 face = "italic", size = 12))
tsplots <- grid.arrange(ts, table,ncol = 2, widths = c(4, 1), clip = FALSE)


g <- ggarrange(maps,tsplots, nrow=2)
g


ggsave(paste0(var,"_ANN.png"), width = 15, height = 9, path = plot.dir)




