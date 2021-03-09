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
library(maptools)


site = "WRST"
aa <- CRS('+init=EPSG:3338') # Alaska Albers
latlong = CRS('+init=EPSG:4326') # Lat/Long


# Base map from Natural Earth - https://www.naturalearthdata.com/downloads/50m-cross-blend-hypso/50m-cross-blended-hypso-with-shaded-relief-and-water/

# topo <- stack('./data/spatial-data/HYP_HR_SR_W/HYP_HR_SR_W/HYP_HR_SR_W.tif') # read in as stack so can see RBG layers
topo <- stack('C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/WRST/2.0 CF and Scenario Development/Historical/HYP_HR_SR_W/HYP_HR_SR_W/HYP_HR_SR_W.tif') # read in as stack so can see RBG layers
ext <- extent(-147, -139, 59.3, 63) # extent defined by lat/long

ak <- crop(topo, ext)
plotRGB(ak)

ak2 <- projectRaster(ak, crs = aa)

#basemap <- ggRGB(ak2)

# NPS Boundary

nps_boundary <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary2018/nps_boundary.shp')
park <- dplyr::filter(nps_boundary, UNIT_CODE == site) # subset to WRST only
park <- st_transform(park, crs = 3338)


Sp_park <- as_Spatial(park)
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:3338") 

plotRGB(ak2)
plot(Sp_park, add = TRUE)

# Boundaries

#lab <- st_read('./data/spatial-data/ne_10m_admin_0_boundary_lines_land')
#lines <- dplyr::filter(lab, adm0_right == "United States of America")
#ak_ca <- lines[2,]

# Raster data

r <- raster('C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/WRST/2.0 CF and Scenario Development/Historical/Rasters/precip_delta.tif')
plot(r)

crs(r) <- aa

# Plot

# Tave color palette = viridis (n = 7)
# Tave delta = Lajolla (n = 7)
# Precip mean = Oslo (n = 7)
# Precip delta = Blues 2 (n = 4)


png('C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/WRST/2.0 CF and Scenario Development/Historical/Maps/Precip_delta.png')

plotRGB(ak2) 
plot(r, col = hcl.colors(n = 4, palette = "Blue-Yellow", alpha = 0.5,rev = TRUE),  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
plot(r, col = hcl.colors(n = 4, palette = "Blue-Yellow", alpha = 0.5, rev = TRUE), legend.only = TRUE, horizontal = TRUE, legend.args = list(text = "Precip (in)", line = 1)) 


dev.off()


