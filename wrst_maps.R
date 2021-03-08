##############################################
###     MAPS      ############################
##############################################

rm(list = ls())

library(rnaturalearth)
library(rnaturalearthdata)
library(sp)
library(sf)
library(ggplot2)
library(rasterVis)
library(colorspace)
library(RStoolbox)


site = "WRST"
aa <- CRS('+init=EPSG:3338') # Alaska Albers
latlong = CRS('+init=EPSG:4326') # Lat/Long


# Base map from Natural Earth - https://www.naturalearthdata.com/downloads/50m-cross-blend-hypso/50m-cross-blended-hypso-with-shaded-relief-and-water/

topo <- stack('C:/Users/adillon/Documents/ArcGIS/HYP_50M_SR_W/HYP_50M_SR_W/HYP_50M_SR_W.tif') # read in as stack so can see RBG layers
ext <- extent(-152, -137, 57, 65) # extent defined by lat/long

ak <- crop(topo, ext)
plotRGB(ak)

ak2 <- projectRaster(ak, crs = aa)

basemap <- ggRGB(ak2)

# NPS Boundary

nps_boundary <- st_read('./data/spatial-data/nps_boundary')
park <- filter(nps_boundary, UNIT_CODE == site) # subset to WRST only
park <- st_transform(park, crs = 3338)


Sp_park <- as_Spatial(park)
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:3338") 

plotRGB(ak2)
plot(Sp_park, add = TRUE)

# Boundaries

lab <- st_read('./data/spatial-data/ne_10m_admin_0_boundary_lines_land')
lines <- dplyr::filter(lab, adm0_right == "United States of America")
ak_ca <- lines[2,]

# Raster data

r <- raster('./output/rasters/Tmean_val.tif')
plot(r)

crs(r) <- aa

# ggplot
# color palette: https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html#overview-1

temp <- gplot(r) + geom_tile(aes(fill = value)) + 
  scale_fill_continuous_divergingx(palette = "Earth") 



basemap +
  geom_sf(data = park) +
  geom_tile(data = r, aes(fill = value)) + 
  scale_fill_continuous_divergingx(palette = "Earth") 

  




map <- levelplot(r, par.settings = BuRdTheme, margin = FALSE, scales = list(draw = FALSE))  


l <-levelplot(r, par.settings = BuRdTheme, margin = FALSE, scales = list(draw = FALSE))  

plotRGB(ak2) + 
  plot(r, col = hcl.colors(n = 7, palette = "viridis", alpha = 0.5), add = TRUE) + 
  plot(Sp_park, add = TRUE)





unique(lab$SUBREGION)

library(tmap)

tm_shape(r) + 
  tm_raster()