###########################################
###   MAKE MAP USING RASTER METHOD  #######
###########################################

library(raster)
library(ncdf4)
library('ncdf4.helpers')
library('maptools') 
library(sf)
library(stars)
library(dplyr)

rm(list = ls())

source("./Code/shift_longitude.R") # to convert negative longitudes to 360 for sf objects

# https://www.cgd.ucar.edu/cms/eaton/netcdf/NCAR-CSM.html # about NCAR netCDF data

# Load data and set projections

######### NetCDF #####################




######  Stars   ###############################################

# https://r-spatial.github.io/stars/reference/read_ncdf.html

s <- read_stars("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", var = c("tmax"))

nc <-read_ncdf("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", curvilinear = c("longitude", "latitude"), var = "tmax")

test <- slice(nc, index = 17, along = "time") # 

# park shapefile 

nps <- st_read('./data/spatial-data/nps_boundary/nps_boundary.shp')
wrst <- dplyr::filter(nps, UNIT_CODE == "WRST")

wrst <- st_transform(wrst, st_crs(test))

wrst <- st_shift_longitude(wrst)

test2 <- st_as_sf(test)

plot(test)
plot(st_geometry(wrst), add = TRUE)


# Convert stars to raster and vice versa

s.raster = as(s, "RasterBrick")
z2 = st_as_stars(z.raster)
all.equal(z, z2, check.attributes = FALSE)
## [1] TRUE





netcdf <- nc_open('./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4')
names(netcdf$dim) # y, x, time
names(netcdf$var) # latitude, longitude, pcp, tmax, tmin

tmax <- names(netcdf$var)[4]

lon <- ncvar_get(netcdf, "longitude")
lat <- ncvar_get(netcdf, "latitude")
ts <- as.POSIXct(nc.get.time.series(netcdf)) # time series

dum_var <- ncvar_get(netcdf, tmax, start=c(1,1,1),count = c(-1,-1,1)) 

nc_close(netcdf)

# Latitude and longitude of grid cell center is located in netcdf$var$latitude

lon <- ncvar_get(netcdf, "longitude") # read in lon
lon <- lon[, 1]

lat <- ncvar_get(netcdf, "latitude")
lat <- lat[1, ]

br <- brick(ncvar_get(netcdf, 'tmax'))
extent(br) <- c(-180,180,-90,90)
crs(br) <- "+init=epsg:3338"

# Park


wrst <- st_transform(wrst, st_crs(br))
wrst <- as_Spatial(wrst) # convert to spatial object so plays nicely with raster

# ----------------------------------------------------------------------- #

# Make sure spatial objects line up

plot(br[[1]])
plot(wrst, add = TRUE)

# Crop

test <- crop(br, wrst)

# Haven't made much headway here can just read in netcdf as a brick and plot it

plot(br[[200]]) #change # and can see that it's changing the date, which defines the layers
z=stack(y)
plot(z[[1]])

crs(Sp_park) <-"+init=epsg:3338"
crs(z) <- "+init=epsg:3338"

r<-mask(y,Sp_park) #subsets but cells are NA, indicating it's wrong cell

bbox<-data.frame(Sp_park@bbox)
bbox[1,]<-bbox[1,] + 365
dfx<-subset(, Lat >= bbox["y","min"] & Lat <= bbox["y","max"] &
              Lon >=bbox["x","min"] & Lon<=bbox["x","max"]) #doesn't work with small parks, need to fix


y.raster<-raster(y[[200]]) # can't figure out how to extract single raster from brick