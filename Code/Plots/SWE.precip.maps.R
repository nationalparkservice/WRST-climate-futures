working.dir <- "C:/Users/achildress/Documents/wrst_temp"

area="WRST_simple"
data.dir <- paste(working.dir, "Data", area, sep="/")

# Need to recalculate SWE.precip with abs for each CF and Daymet for wrst-simple

# read in WRST-simple SWE.precip abs
# create CF file lists
var="SWE.precip_abs"
rds.ls = list.files(path = data.dir, pattern = paste0(var,"_"), full.names = TRUE)
Hist = Filter(function(x) grepl("daymet", x), rds.ls)
CF1.ls = Filter(function(x) grepl(paste(GCMs[1], collapse = "|"), x), rds.ls)
CF2.ls = Filter(function(x) grepl(paste(GCMs[2], collapse = "|"), x), rds.ls)
CF3.ls = Filter(function(x) grepl(paste(GCMs[3], collapse = "|"), x), rds.ls)
# read in RDS for setting scale limits
hist <- readRDS(Hist)
cf1 <- readRDS(CF1.ls)
cf2 <- readRDS(CF2.ls)
cf3 <- readRDS(CF3.ls)

# create maps for whole area
boundary.dir <- "C:/Users/achildress/DOI/NPS-WRST-Resource Stewardship Strategy - Climate/3.0 Climate futures development (Summer 2021)/Data/Boundary_shapefiles/"
wrst <- st_read(paste0(boundary.dir, "wrst_simple.shp")) # Wrangell Mountains
wrst <- st_transform(wrst, 3338)

# dem = raster(paste0(boundary.dir,"/DEM/dem_3338")) # crs = 'SR-ORG:80'
# projectRaster(dem,wrst)

# ak_df  <- as.data.frame(ak2, xy = TRUE) # this step is important to get it to plot in ggplot

mean_grid<-st_apply(ratio, c("x", "y"), mean)

mean_grid %>% 
  mutate(categories = round(mean,digits=1),
    cat_factor = factor(categories, levels=seq(0.1,1,0.1))) %>% 
    dplyr::select(cat_factor) -> cat

# Split by geog, add in DEM, convert to df

# fill plot (heatmap?) with aes(x=time,y=elev,fill=SWE.p)

col.ramp = c("mediumseagreen","#fed976", "#fd8d3c", "#e31a1c", "#c6dbef",
             "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594")

# ggplot
# map.plot <- function(data, title,xaxis,metric,col){
  ggplot() + 
    # geom_raster(data = ak_df ,aes(x = x, y = y,alpha=HYP_HR_SR_W.1), show.legend=FALSE) +
    geom_stars(data = cat, alpha = 0.8) + 
    geom_sf(data = wrst, aes(), fill = NA) + 
    labs(title = "snow:rain") +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.spacing = unit(0, "cm"),
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = "black", fill=NA, size=5)) + 
    labs(fill = paste0("snow:rain")) +
    scale_fill_manual(values = col.ramp,
                      limits = c("0.1", "0.2","0.3", "0.4","0.5", "0.6","0.7", "0.8", "0.9", "1.0")) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE,label.position="top",title.position = "top",title.hjust = 0.5))
         
# }

cf1.plot <- map.plot(data=readRDS(CF1.ls),title=CFs[1],metric=long.title,col=cols[1])
cf2.plot <- map.plot(data=readRDS(CF2.ls),title=CFs[2],metric=long.title,col=cols[2])
cf3.plot <- map.plot(data=readRDS(CF3.ls),title=CFs[3],metric=long.title,col=cols[3])

############## ts plots
as.data.frame(ratio1) -> c
c2 <- gather(c,year,val,X1980.01.01:X2016.01.01) %>% 
  drop_na() %>% 
  mutate(Year = substr(year, 2, 5),
         categories = round(val,digits=1),
         categories = ifelse(categories < 0.1, 0.1,ifelse(categories>1,1,categories)),
         ) %>% 
  group_by(categories, Year) %>% 
  summarize(sum_cat = sum(categories)) %>%
  group_by(Year) %>%
  mutate(percent = sum_cat/sum(sum_cat)) %>%
  mutate(cat_factor = factor(categories, levels=seq(0.1,1,0.1))) %>% 
  arrange(Year)

ggplot(c2, aes(fill=cat_factor, y=percent, x=Year)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = col.ramp)
                    
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




