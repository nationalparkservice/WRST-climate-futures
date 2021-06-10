################################################
###   SPATIAL SUBSET USING STARS AND SF  #######
################################################

# https://www.cgd.ucar.edu/cms/eaton/netcdf/NCAR-CSM.html # about NCAR netCDF data

#library(raster)
#library(ncdf4)
#library('ncdf4.helpers')
#library('maptools') 
library(sf)
library(stars)
library(dplyr)

# Stars objects can be subsetted by specifying dimensions/attributes like this:

  # first argument selects attributes
  # second argument selects first dimension
  # third argument selects second dimension

# Stars objects can also be subsetted spatially using a shapefile, as this script demonstrates

rm(list = ls())

source("./Code/shift_longitude.R") # to convert negative longitudes to 360 for sf objects

# -----  LOAD DATA  ------------ #

# https://r-spatial.github.io/stars/reference/read_ncdf.html

nc <-read_ncdf("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", 
               curvilinear = c("longitude", "latitude"), var = "tmax") # Could not get this to work using read_stars


# ---- TEST SPATIAL SUBSETTING OF STARS OBJECT --------------- #

test <- slice(nc, index = 17, along = "time") # Random slice just to see if plotting/subsetting works

# park shapefile 

nps <- st_read('./data/spatial-data/nps_boundary/nps_boundary.shp')
wrst <- dplyr::filter(nps, UNIT_CODE == "WRST")

wrst <- st_shift_longitude(wrst) # to convert negative longitudes to 360 degree longitudes (to match NCAR data) https://rdrr.io/cran/sf/src/R/shift_longitude.R

# Plot

# Plot works without applying projection, but can also project as desired (see below)

plot(test, border = NA, reset = FALSE) # reset = FALSE keeps the plot 'open' for more layers
plot(st_geometry(wrst), add = TRUE) 


wrst <- st_transform(wrst, 3338) # NAD83 Alaska Albers
test <- st_transform(test, 3338)

# But also looks fine when projected

plot(test, border = NA, reset = FALSE) # reset keeps the plot 'open' for adding layers. 
plot(st_geometry(wrst), add = TRUE)

plot(test[wrst])

# Create new object by spatially subsetting stars object using shapefile

ex <- test[wrst]
plot(ex)


