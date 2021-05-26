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
library(RColorBrewer)


site = "WRST"
aa <- CRS('+init=EPSG:3338') # Alaska Albers
latlong = CRS('+init=EPSG:4326') # Lat/Long

PlotIn <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/WRST/2.0 CF and Scenario Development/Historical/Rasters/"
PlotOut <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/WRST/2.0 CF and Scenario Development/Historical/Maps/"


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


################################# PLOTS #############################################

###################################
# Precipitation
###################################
# Mean
r <- raster(paste(PlotIn,'Precip_mean.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Precip mean
col=hcl.colors(n = 9, palette = "Oslo", alpha = 0.7)[3:9]
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Precip_mean.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = "Precip (in)", line = 1)) 

dev.off()


########################
# Delta

r <- raster(paste(PlotIn,'precip_delta.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Precip delta
col=hcl.colors(n = 7, palette = "Blue-Yellow", alpha = 0.7, rev=TRUE)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Precip_delta.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = "Precip (in)", line = 1)) 

dev.off()


###################################
# Tmax
###################################
# Mean
r <- raster(paste(PlotIn,'Tmax_val.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp mean
col=hcl.colors(n = 7, palette = "Viridis", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmax_mean.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text =  expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


########################
# Delta

r <- raster(paste(PlotIn,'tmax_delta.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp delta
col=hcl.colors(n = 7, palette = "Lajolla", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmax_delta.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


###################################
# Tmin
###################################
# Mean
r <- raster(paste(PlotIn,'Tmin_val.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp mean
col=hcl.colors(n = 7, palette = "Viridis", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmin_mean.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text =  expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


########################
# Delta

r <- raster(paste(PlotIn,'tmin_delta.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp delta
col=hcl.colors(n = 7, palette = "Lajolla", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmin_delta.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


###################################
# Tmean
###################################
# Mean
r <- raster(paste(PlotIn,'Tmean_val.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp mean
col=hcl.colors(n = 7, palette = "Viridis", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmean_mean.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text =  expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


########################
# Delta

r <- raster(paste(PlotIn,'tmean_delta.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp delta
col=hcl.colors(n = 7, palette = "Lajolla", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmean_delta.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


