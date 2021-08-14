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
library(ggthemes)
library(tmap)
library(tmaptools)
library(units)
library(viridis)


rm(list = ls())

source("./Code/shift_longitude.R") # to convert negative longitudes to 360 for sf objects

# ----  LOAD DATA ------------------------------- #

# NetCDF - it's fine to load as netcdf but have not found advantages to doing so over stars object

#nc <-read_ncdf("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", 
               #curvilinear = c("longitude", "latitude"), var = "tmax") 

#test <- slice(nc, index = 17, along = "time") 

# Stars

st <- read_stars("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", sub = "tmax", curvilinear = c("longitude", "latitude"))
st



# try writing to file

write_stars(st, "test.nc4")
read_stars("test.nc4") # works

st1 <- st_set_dimensions(st, 3, values = as.character(st_get_dimension_values(st, 3))) # In st1, time now has a value. Not sure what is gained by this, but seems like might be important to know. 
st1


# I do not see a difference between reading in as stars v. ncdf. 
#Vignette says read_ncdf does not convert time values in proper R format. 


# Park shapefile

nps <- st_read('./data/spatial-data/nps_boundary/nps_boundary.shp')
wrst <- dplyr::filter(nps, UNIT_CODE == "WRST")

wrst <- st_shift_longitude(wrst) 

# Project 

wrst <- st_transform(wrst, 3338) # NAD83 Alaska Albers
st<- st_transform(st, 3338)

# To remove units
# Problem is this removes all units, including lat/long

# Solves some units issues: https://github.com/r-spatial/mapview/issues/338

drop_units(st[[1]]) -> st2 # st[[1] is where units are defined

st2 <- st_as_stars(st2) # turn back into stars object, now there are no units but attribute has lost its name. Also lat/long have lost units. 

st2 <- setNames(st2, "tmax") # rename attribute

# mutate - create an attribute (temp in Fahrenheit)

st2 %>% mutate(tmaxF = tmax * (9/5) + 32) -> stF

st %>% mutate(tmaxF = tmax * (9/5) + 32) -> test

# select - can select attributes, i.e. tempC or tempF

stF %>% select(tmaxF) -> tmaxF # now tempF only

stF %>% slice(time, 100) -> day100F

plot(day100F) # plots upside-down but still looks ok. Maybe later look into preserving lat/long (or reassigning) through units functions. 


# ----- DPLYR FUNCTIONS ----------------------------------------------------- #

# Aggregate

# First crop

st_wrst <- st[wrst]

# See what plot of AK looks like when cropped

ggplot() + 
  geom_stars(data = st_wrst, alpha = 0.8) + 
  #facet_wrap("time") + 
  scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))

by_t = "1 year"

test <- aggregate(st_wrst, by = by_t, FUN = mean, na.omit = TRUE) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
test

plot(test)

test2 <- split(test, "time") # ggplot will not work if time is a dimension, so switching to an attribute. Should not matter since time is aggregated here. 

# Plot

ggplot() + # Resolution is course
  geom_stars(data = test2, alpha = 0.8) + 
  #facet_wrap("time") + 
  scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))


# st_apply

test4 <- st_apply(st_wrst, c("x", "y"), mean) # mean value for each pixel
test4

plot(test4)
plot(test4, as_points = FALSE, axes = TRUE, breaks = "equal", border = NA)


ggplot() + # same as aggregate; low resolution
  geom_stars(data = test4, alpha = 0.8) + 
  #facet_wrap("time") + 
  scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))


