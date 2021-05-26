##############################################################
##    EXTRACT NOAA nClimGrid DATA FOR ALASKA   ###############
##############################################################

# How untar multiple files for NOAA data
library(sf)
library(dplyr)
library(raster)

library(rgdal)
library(plyr)
library(ggplot2)

rm(list=ls())

# Initials
data.dir <- "D:/nClimGrid" #location for text files

# spatial data
##############################################################################
# initials
site = "WRST"

# read in parks shapefile
nps_boundary <- st_read('./data/spatial-data/nps_boundary')
park <- filter(nps_boundary, UNIT_CODE == site) # subset to WRST only


# park <- st_transform(park, st_crs(epsg))
Sp_park <- as_Spatial(park)
# set to WGS84

bbox<-data.frame(Sp_park@bbox) # get bounding box

# create stack for each var by pattern - "prcp.alaska" [AK not for whole period]
pnt.list<-list.files(path=data.dir, pattern=".prcp.alaska") #list all files by var

# read in, create df, subset by max lat/lon
df1 <- read.table(paste(data.dir,pnt.list[1],sep="/"))

colnames(df1)=c("Lat","Lon","PrecipC")

dfx<-subset(df1, Lat >= bbox["y","min"] & Lat <= bbox["y","max"] &
              Lon >=bbox["x","min"] & Lon<=bbox["x","max"]) #doesn't work with small parks, need to fix

# create SP obj
coordinates(dfx) <- ~Lon+Lat
proj4string(dfx) <- "+proj=longlat +datum=WGS84 +no_defs " #same proj4string used in NPS_boundary_centroids.shp

# Transform to espg3338
dfx<-spTransform(dfx,CRSobj = "+init=epsg:3338") #reproj sp obj

#rasterize
y<-data.frame(dfx@coords)
y$PrecipC<-dfx$PrecipC

Dfx<-as.matrix(y)
e<-extent(Dfx[,1:2])
r <- raster(e, ncol=85, nrow=71)
x <- rasterize(Dfx[, 1:2], r, Dfx[,3])
plot(x)

projection(x) = CRS("+init=epsg:3338")

# Plot park over raster
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:3338") #reproj sp obj

# reproj raster
X<-projectRaster(x,crs=crs(Sp_park))

plot(x)
plot(Sp_park,add=TRUE)

writeRaster(x,paste(tar.dir,'WRST.tif',sep="/"),options=c('TFW=YES'))
