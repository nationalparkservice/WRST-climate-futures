## MET MONTHLY
for (G in 1:length(GCMs)){
  # setting variables ----
  gcm = sub("\\..*", "", GCMs[G])
    rcp = sub('.*\\.', '', GCMs[G])
    path = paste(met.dir,"monthly/BCSD", gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
    hist_filelist = Filter(function(x) grepl(paste(historical.period, collapse = "|"), x), file.list)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    
    print(paste0("extracting ", GCMs[G]))

    # Createing stars objects ####

      # HISTORICAL ----

    l <- list() # Create a list to put the stars objects into

    for(i in 1:length(hist_filelist)){
      suppressMessages(
      l[[i]] <- read_ncdf(hist_filelist[i], curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
      )
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

    # assign(paste0("cropped_st_hist_",GCMs[G]), cropped_st_hist)
    saveRDS(cropped_st_hist, file = paste(data.dir,paste0("cropped_st_hist_",gcm,"_",rcp),sep="/"))


      # FUTURE ----
    
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(fut_filelist)){
      suppressMessages(
      l[[i]] <- read_ncdf(fut_filelist[i], curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
      )
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
    # assign(paste0("cropped_st_fut_",GCMs[G]), cropped_st_fut)
    saveRDS(cropped_st_fut, file = paste(data.dir,paste0("cropped_st_fut_",gcm,"_",rcp),sep="/"))
}
print("extracting Daymet")


grid_filelist = list.files(path = paste(met.dir,"monthly/daymet",sep='/'), pattern= '.nc', full.names = TRUE)

# DAYMET ----

l <- list() # Create a list to put the stars objects into

for(i in 1:length(grid_filelist)){
  invisible(capture.output(
    suppressWarnings(
      l[[i]] <- read_stars(grid_filelist[i], curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
    )))
}

# Crop

cropped_grid <- list() # create list for cropped stars objects

for(i in 1:length(l)){ # add cropped stars objects to a new list
  nc = l[[i]]
  nc = st_transform(nc, st_crs(shp))
  nc_crop = nc[shp]
  cropped_grid[[i]] = nc_crop
}

cropped_st_grid <- list()

for(i in 1:length(cropped_grid)){
  cropped_st_grid[[i]] <- st_as_stars(cropped_grid[[i]])
}
# assign(paste0("cropped_st_grid_",GCMs[G]), cropped_st_grid)
saveRDS(cropped_st_grid, file = paste(data.dir,"cropped_st_Daymet",sep="/"))


rm(cropped_st_grid,cropped_st_fut,cropped_fut,cropped_grid,nc_crop,nc,l)

  