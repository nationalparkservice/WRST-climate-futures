library(ncdf4)
library(reshape2)
library(raster)


rm(list=ls())

Lat = 62.23494
Lon = -142.572

#lat = 209 values; lon = 299 values

# MET parsing
data.dir<-"C:/Users/achildress/Documents/NCAR-test/met/"

x<-nc_open(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"))

# all_lat<-data.frame(x$dim$lat$vals)

ncvar_get(x,"latitude")

Lat_index = as.numeric(which.min(abs(All_lat$nc1.dim.lat.vals - Lat)))

lon <- ncvar_get(x, "longitude")
lat <- ncvar_get(x, "latitude")
la<-data.frame(lat) # row 150 is max, after that mirrors values
lo<-data.frame(lon) #

Lon_index = as.numeric(which.min(abs(lon - Lon)))
Lat_index = as.numeric(which.min(abs(lat - Lat)))

y = ncvar_get(x, "tmax", c(Lon_index, Lat_index, 1))

y = brick(ncvar_get(x, "tmax"),crs="+init=epsg:3338")
y.raster<-raster(y[[200]])
crs(y) <- "+proj=longlat +datum=WGS84 +no_defs"
crs(y) <- "+init=epsg:3338"
projectRaster(y,crs = "+init=epsg:3338")

rast.lon<-raster(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"),varname="longitude")
y2<-projectRaster(y[[1]],crs = "+init=epsg:3338") #reproj sp obj
y[[1]]
