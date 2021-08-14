################################################
###   SPATIAL SUBSET USING STARS AND SF  #######
################################################

# Stars object can be subsetted spatially using a shapefile or by cell indices

# https://www.cgd.ucar.edu/cms/eaton/netcdf/NCAR-CSM.html # about NCAR netCDF data

library(sf)
library(stars)
library(dplyr)

rm(list = ls())


######## SUBSET STARS OBJECT BY SHAPEFILE #################################################################

source("./Code/shift_longitude.R") # to convert negative longitudes to 360 for sf objects

# -----  LOAD DATA  ------------ #

# https://r-spatial.github.io/stars/reference/read_ncdf.html

nc <-read_ncdf("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", 
               curvilinear = c("longitude", "latitude"), var = "tmax") # Could not get this to work using read_stars


# ---- TEST SPATIAL SUBSETTING OF STARS OBJECT --------------- #

test <- slice(nc, index = 17, along = "time") # Random slice just to see if plotting/subsetting works

# park shapefile 

nps <- st_read('./data/spatial-data/nps_boundary/nps_boundary.shp') # where file is located on Annie's computer
wrst <- dplyr::filter(nps, UNIT_CODE == "WRST")

# saveRDS(wrst, file = 'data/wrst.Rds') # save wrst shapefile to repo so anyone can access it
# wrst <- readRDS('./data/wrst.Rds')

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

#### SUBSET STARS OBJECT USING CELL INDICES ##########################################################################

# Stars objects can be subsetted by specifying dimensions/attributes like this:

# first argument selects attributes
# second argument selects first dimension
# third argument selects second dimension and so forth

st <- read_stars("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", sub = "tmax", curvilinear = c("longitude", "latitude")) # I don't know whether this will work when read in using read_ncdf
st

# slice - slices a sub-array out of the cube; this is done by specifying the dimension on which to act, and slice number
# MUST BE DIMENSION, NOT ATTRIBUTE

st %>% slice(time, 100) -> day100
day100

# Subset using indices

day100["tmax", 1:299, 1:100] -> sub # WORKS! But we have to figure out how cell indexes correspond to lat and long
plot(sub)
