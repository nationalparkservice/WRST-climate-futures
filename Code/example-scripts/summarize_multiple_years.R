#############################################################################
###   SUMMARIZING ACROSS MULTIPLE YEARS   ###################################
#############################################################################


library(sf)
library(stars)
library(dplyr)
library(tidyr)
library(ncmeta)
library(ggplot2)
library(ggthemes)
library(viridis)


rm(list = ls())

memory.limit(size = 60000) # not sure if memory limit resets when R is closed. 60000 is approximately doubling the size

source('./Code/shift_longitude.R')

# ------- Read in data from multiple files  ----------------------- #

file_list = list.files(path = "C:/Users/adillon/Documents/RSS/WRST/Test/met/Historical", pattern = '.nc4', full.names = TRUE)

l <- list() # Create a list of stars objects
for(i in 1:length(file_list)){
  l[[i]] <- read_ncdf(file_list[i], var = "tmax", curvilinear = c("longitude", "latitude"), proxy = TRUE) # need to read in as ncdf or coordinate system does not translate (not sure why)
}

# ------  Crop stars objects to wrst  ----------------------------- #

nps <- st_read('./data/spatial-data/nps_boundary/nps_boundary.shp')
wrst <- dplyr::filter(nps, UNIT_CODE == "WRST")
wrst <- st_shift_longitude(wrst) # From function in global env
wrst <- st_transform(wrst, 3338) # NAD83 Alaska Albers

cropped <- list() # create list for cropped stars objects

system.time( # 1 min 27 sec
for(i in 1:length(l)){ # add cropped stars objects to a new list
  nc = l[[i]]
  nc = st_transform(nc, st_crs(wrst))
  nc_crop = nc[wrst]
  cropped[[i]] = nc_crop
}
)

# Check that cropping worked

#crop_test <- cropped[[5]]
#crop_test %>% slice(time, 25) -> crop_test # 25 = 25th day of the year of the first year in the dataset (1950)
#plot(crop_test) # works

# -------   Combine all historical stars objects into a single stars object ------------- #

# Convert to stars object

cropped_st <- list()

for(i in 1:length(cropped)){
  cropped_st[[i]] <- st_as_stars(cropped[[i]])
}

rm(crop_test, cropped, l, nc, nc_crop, nps, wrst) # remove large objects to free up memory


system.time( # ~ 8 min
hist <- Reduce(c, cropped_st)
)

hist # looks good!

#write_stars(hist, "./data/hist_MIROC5") # write to file in case it requires less memory to load from file when doing calculations. Also maybe can load as a proxy object? Not sure.

system.time( # ~3.5 min
test <- st_apply(hist, c("x", "y"), mean) # mean value for each pixel
)

# Plot

ggplot() + 
  geom_stars(data = test, alpha = 0.8) + 
  #facet_wrap("time") + 
  scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))

# Compare to PRISM data from https://prism.oregonstate.edu/projects/public/alaska/graphics/tmax/
# Looks good


# ---- FUTURE ------------------------------------------------------ #

rm(cropped_st, hist)

file_list = list.files(path = "C:/Users/adillon/Documents/RSS/WRST/Test/met/Future", pattern = '.nc4', full.names = TRUE) # 2025-2055

l <- list() # Create a list of stars objects
for(i in 1:length(file_list)){
  l[[i]] <- read_ncdf(file_list[i], var = "tmax", curvilinear = c("longitude", "latitude"), proxy = TRUE) # need to read in as ncdf or coordinate system does not translate (not sure why)
}

nps <- st_read('./data/spatial-data/nps_boundary/nps_boundary.shp')
wrst <- dplyr::filter(nps, UNIT_CODE == "WRST")
wrst <- st_shift_longitude(wrst) # From function in global env
wrst <- st_transform(wrst, 3338) # NAD83 Alaska Albers

cropped <- list() # create list for cropped stars objects

for(i in 1:length(l)){ # add cropped stars objects to a new list
    nc = l[[i]]
    nc = st_transform(nc, st_crs(wrst))
    nc_crop = nc[wrst]
    cropped[[i]] = nc_crop
  }

cropped_st <- list()

for(i in 1:length(cropped)){
  cropped_st[[i]] <- st_as_stars(cropped[[i]])
}

rm(crop_test, cropped, l, nc, nc_crop, nps, wrst) # remove large objects to free up memory


fut <- Reduce(c, cropped_st) # works!

write_stars(fut, "./data/fut_MIROC5") # write to file in case it requires less memory to load from file when doing calculations. Also maybe can load as a proxy object? Not sure.

fut_mean <- st_apply(fut, c("x", "y"), mean) # mean tmax value for each pixel. The aggregate function also works, I think identically (see simple_summaries.R)

change <- fut_mean - test

ggplot() + # same as aggregate; low resolution
  geom_stars(data = change, alpha = 0.8) + 
  #facet_wrap("time") + 
  scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))
