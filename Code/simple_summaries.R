####################################################################
#####     SIMPLE TEMP/PRECIP SUMMARIES USING STARS    ##############
####################################################################

# Stars vignette: https://r-spatial.github.io/stars/articles/


library(sf)
library(stars)
library(dplyr)
library(tidyr)
library(ncmeta)
library(ggplot2)
library(tmap)
library(tmaptools)


rm(list = ls())

source("./Code/shift_longitude.R") # to convert negative longitudes to 360 for sf objects

# ----  LOAD DATA ------------------------------- #

# NetCDF

nc <-read_ncdf("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", 
               curvilinear = c("longitude", "latitude"), var = "tmax") 

test <- slice(nc, index = 17, along = "time") 

# Stars

st <- read_stars("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", sub = "tmax", curvilinear = c("longitude", "latitude"))
st

st1 <- st_set_dimensions(st, 3, values = as.character(st_get_dimension_values(st, 3))) # In st1, time now has a value
st1



# I do not see a difference between reading in as stars v. ncdf. 
#Vignette says read_ncdf does not convert time values in proper R format. Both look to be POSIXct.


# Park shapefile

nps <- st_read('./data/spatial-data/nps_boundary/nps_boundary.shp')
wrst <- dplyr::filter(nps, UNIT_CODE == "WRST")

wrst <- st_shift_longitude(wrst) 

# Project 

wrst <- st_transform(wrst, 3338) # NAD83 Alaska Albers
st<- st_transform(st, 3338)

# To remove units

drop_units <- function(x) { 
  class(x) <- setdiff(class(x), "units")
  attr(x, "units") <- NULL
  x
}

drop_units(st[[1]]) -> st2 # st[[1] is where units are defined

st2 <- st_as_stars(st2) # turn back into stars object, now there are no units but attribute has lost its name

st2 <- setNames(st2, "tmax") # name attribute

# mutate - create an attribute that is temperature in Fahrenheit

st2 %>% mutate(tmaxF = tmax * (9/5) + 32) -> stF









# Aggregate

by_t = "1 year"

test <- aggregate(st_wrst, by = by_t, FUN = mean, na.omit = TRUE)
test

plot(test)

# Remove NA

#test %>% drop_na() -> NAtest # doesn't work

NAtest <- na.omit(test)

NAtest


# Map with tmap

tmap_mode("view")

tm_shape(test, raster.warp = FALSE) + 
  tm_raster(midpoint = NA) + 
  tm_facets(as.layers = TRUE)


test[is.na(test)] <- 999 # I think the NA's are causing issues. Trying to figure out how to remove them. 



