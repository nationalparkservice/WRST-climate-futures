library(raster)
vic.dir <- "D:/NCAR_AK/vic_hydro"

working.dir <- "C:/Users/achildress/Documents/wrst_temp"

area="WRST_simple"
data.dir <- paste(working.dir, "Data", area, sep="/")

# Need to recalculate SWE.precip with abs for each CF and Daymet for wrst-simple

# read in WRST-simple SWE.precip abs
# create CF file lists
var="SWE.precip"
rds.ls = list.files(path = data.dir, pattern = paste0(var,"_"), full.names = TRUE)
CF1.ls = Filter(function(x) grepl(paste(GCMs[1], collapse = "|"), x), rds.ls)
CF2.ls = Filter(function(x) grepl(paste(GCMs[2], collapse = "|"), x), rds.ls)
CF3.ls = Filter(function(x) grepl(paste(GCMs[3], collapse = "|"), x), rds.ls)
# read in RDS for setting scale limits
cf1 <- readRDS(CF1.ls)
cf2 <- readRDS(CF2.ls)
cf3 <- readRDS(CF3.ls)

# create maps for whole area
boundary.dir <- "C:/Users/achildress/DOI/NPS-WRST-Resource Stewardship Strategy - Climate/3.0 Climate futures development (Summer 2021)/Data/Boundary_shapefiles/"
wrst <- st_read(paste0(boundary.dir, "wrst_simple.shp")) # Wrangell Mountains
wrst <- st_transform(wrst, 3338)

dem = raster(paste0(boundary.dir,"/DEM/dem_3338")) # crs = 'SR-ORG:80'
projectRaster(dem,wrst)

ak_df  <- as.data.frame(ak2, xy = TRUE) # this step is important to get it to plot in ggplot


cf1 %>% 
  mutate(categories = case_when(
    air_temperature < 3.9  ~ "Snow",
    air_temperature > 3.9 & air_temperature < 3.95 ~ "Mix",
    air_temperature > 3.95  ~ "Precip"),
    categories = factor(categories, levels=c("Snow","Mix","Precip"))
  )  %>% select(categories) -> cat_crop

# Split by geog, add in DEM, convert to df

# fill plot (heatmap?) with aes(x=time,y=elev,fill=SWE.p)






scale.min = min(c(cf1$mean,cf2$mean,cf3$mean),na.rm=TRUE)
scale.max = max(c(cf1$mean,cf2$mean,cf3$mean),na.rm=TRUE)

# ggplot
map.plot <- function(data, title,xaxis,metric,col){
  ggplot() + 
    geom_raster(data = ak_df ,aes(x = x, y = y,alpha=HYP_HR_SR_W.1), show.legend=FALSE) +
    geom_stars(data = data, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    scale_fill_viridis(direction=1, option = scale,
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5),
                       limits = c(scale.min, scale.max), oob = scales::squish) + #mako for WB delta
    labs(title = title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, size=5)) + 
    labs(fill = paste0("Change in ",metric))
}

cf1.plot <- map.plot(data=readRDS(CF1.ls),title=CFs[1],metric=long.title,col=cols[1])
cf2.plot <- map.plot(data=readRDS(CF2.ls),title=CFs[2],metric=long.title,col=cols[2])
cf3.plot <- map.plot(data=readRDS(CF3.ls),title=CFs[3],metric=long.title,col=cols[3])

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
  coord_fixed(ratio = ratio) 
ts


#### Just maps and ts plot
maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot, cf3.plot, ncol = 3, nrow = 1, position = "bottom", 
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




