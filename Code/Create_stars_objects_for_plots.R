############################################################################
##    CREATE STARS OBJECTS FOR T1 PLOTS   ##################################
############################################################################

# shp = whatever shapefile you want to use to crop the area of interest

# HISTORICAL

l <- list() # Create a list to put the stars objects into

for(i in 1:length(hist_filelist)){
  l[[i]] <- read_ncdf(hist_filelist[i], curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
}

# Crop

cropped_hist <- list() # create list for cropped stars objects

for(i in 1:length(l)){ # add cropped stars objects to a new list
  nc = l[[i]]
  nc = st_transform(nc, st_crs(shp))
  nc_crop = nc[shp]
  cropped_hist[[i]] = nc_crop
}

cropped_st_hist <- list()

for(i in 1:length(cropped_hist)){
  cropped_st_hist[[i]] <- st_as_stars(cropped_hist[[i]])
}

# FUTURE

l <- list() # Create a list to put the stars objects into

for(i in 1:length(fut_filelist)){
  l[[i]] <- read_ncdf(fut_filelist[i], curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
}

# Crop

cropped_fut <- list() # create list for cropped stars objects

for(i in 1:length(l)){ # add cropped stars objects to a new list
  nc = l[[i]]
  nc = st_transform(nc, st_crs(shp))
  nc_crop = nc[shp]
  cropped_fut[[i]] = nc_crop
}

cropped_st_fut <- list()

for(i in 1:length(cropped_fut)){
  cropped_st_fut[[i]] <- st_as_stars(cropped_fut[[i]])
}
