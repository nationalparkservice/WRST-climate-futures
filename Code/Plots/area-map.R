library(raster)
# Spatial
boundary.dir <- "C:/Users/achildress/DOI/NPS-WRST-Resource Stewardship Strategy - Climate/3.0 Climate futures development (Summer 2021)/Data/Boundary_shapefiles/"
# plot(st_geometry(shp))

wrst_mtns <- st_read(paste0(boundary.dir, "wrst_mtns.shp")); wrst_mtns <- st_transform(wrst_mtns, 3338)
copper_pleatau <- st_read(paste0(boundary.dir, "copper_plateau.shp")); copper_pleatau <- st_transform(copper_pleatau, 3338)
coastal_mtns <- st_read(paste0(boundary.dir, "coastal_mtns.shp")); coastal_mtns <- st_transform(coastal_mtns, 3338)
eastern_coast <- st_read(paste0(boundary.dir, "eastern_coast.shp")); eastern_coast <- st_transform(eastern_coast, 3338)

topo <- stack(paste0(boundary.dir, 'HYP_HR_SR_W/HYP_HR_SR_W.tif')) # read in as stack so can see RBG layers
ext <- extent(-147, -139, 59.3, 63) # extent defined by lat/long
ak <- crop(topo, ext)
# plotRGB(ak)
ak2 <- projectRaster(ak, crs = CRS('+init=EPSG:3338')) # Alaska Albers 
ak_df  <- as.data.frame(ak2, xy = TRUE) # this step is important to get it to plot in ggplot

# Read in RDS 
grid <- readRDS(paste0(data.dir,"/WRST_simple/Annual.precipIn_ACCESS1-3_rcp45"))

ggplot() +
  geom_raster(data = ak_df ,aes(x = x, y = y,alpha=HYP_HR_SR_W.1), show.legend=FALSE) + #it's a multiband raster so alpha is band I wanted to use
  geom_stars(data = readRDS(paste0(boundary.dir,'r.rds')), alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
  geom_sf(data = wrst, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the park I'm plotting
  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
  # scale_fill_viridis(direction=-1, option = "C", #begin = .5, end = 1,
                     # guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
  labs(title = "ACCESS1-3 RCP 4.5") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5)) +
  # plot.background = element_rect(colour = col, fill=NA, size=5)) +
  # labs(fill = "Water Balance") +
  scale_colour_manual(values = c(rgb(207, 31, 46, maxColorValue = 255)), "#ffda85")
