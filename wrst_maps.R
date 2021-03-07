##############################################
###     MAPS      ############################
##############################################

rm(list = ls())

library(rnaturalearth)
library(sp)

topo <- stack('C:/Users/adillon/Documents/ArcGIS/HYP_50M_SR_W/HYP_50M_SR_W/HYP_50M_SR_W.tif')
ext <- extent(-168, -143, 57, 65)

ak <- crop(topo, ext)
plotRGB(ak)

r <- raster('./output/rasters/Tmean_val.tif')
plot(r)


library(tmap)

tm_shape(r) + 
  tm_raster()