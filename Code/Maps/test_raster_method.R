###########################################
###   MAKE MAP USING RASTER METHOD  #######
###########################################

# https://www.cgd.ucar.edu/cms/eaton/netcdf/NCAR-CSM.html # about NCAR netCDF data

library(raster)
library(ncdf4)
library('ncdf4.helpers')
library('maptools') 
library(sf)
library(stars)
library(dplyr)

rm(list = ls())

source("./Code/shift_longitude.R") # to convert negative longitudes to 360 for sf objects

# -----  Load data ------------ #

# https://r-spatial.github.io/stars/reference/read_ncdf.html

nc <-read_ncdf("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", 
               curvilinear = c("longitude", "latitude"), var = "tmax") # Could not get this to work using read_stars

test <- slice(nc, index = 17, along = "time") # Random slice just to see if plotting works

# park shapefile 

nps <- st_read('./data/spatial-data/nps_boundary/nps_boundary.shp')
wrst <- dplyr::filter(nps, UNIT_CODE == "WRST")

wrst <- st_shift_longitude(wrst) # to convert negative longitudes to 360 degree longitudes (to match NCAR data) https://rdrr.io/cran/sf/src/R/shift_longitude.R

#wrst <- st_transform(wrst, st_crs(test))

# Plot

# Plot works without adjusting projection!!!!

plot(test, border = NA, reset = FALSE) # reset = FALSE keeps the plot 'open' for more layers
plot(st_geometry(wrst), add = TRUE) 


wrst <- st_transform(wrst, 3338)
test <- st_transform(test, 3338)

# But also looks fine when projected

plot(test, border = NA, reset = FALSE)
plot(st_geometry(wrst), add = TRUE)




