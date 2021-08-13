## VIC MONTHLY
for (G in 1:length(GCMs)){
  # setting variables ----
  gcm = sub("\\..*", "", GCMs[G])
    rcp = sub('.*\\.', '', GCMs[G])
    path = paste(vic.dir,"monthly/BCSD", gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
    hist_filelist = Filter(function(x) grepl(paste(historical.period, collapse = "|"), x), file.list)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    
    # model.dir <- paste0(data.dir,"/",gcm,".",rcp)
    # dir.create(model.dir,showWarnings=FALSE)
    
    print(paste0("extracting ", GCMs[G]))

    # Createing eb stars objects ####

    eb_hist_filelist <- hist_filelist[grep("eb", hist_filelist)]
    eb_fut_filelist <- fut_filelist[grep("eb", fut_filelist)]
    
    
    # HISTORICAL
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(eb_hist_filelist)){
      invisible(capture.output(
        suppressWarnings(
          (l[[i]] <- read_stars(eb_hist_filelist[i], sub = c("SOIL_TEMP1","SOIL_TEMP2","SOIL_TEMP3"), curvilinear = c("longitude", "latitude"))) 
        )))
    }
    # Warnings have been suppressed because they do not impact the numerical results. See here:
    # https://gis.stackexchange.com/questions/379890/decreasing-resolution-of-netcdf-data-with-gdal
    
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
    saveRDS(cropped_st_hist, file = paste(data.dir,paste0("cropped_st_hist_eb_",gcm,"_",rcp),sep="/"))
    
    # FUTURE
    
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(eb_fut_filelist)){
      invisible(capture.output(
        suppressWarnings(
          l[[i]] <- read_stars(eb_fut_filelist[i], sub = c("SOIL_TEMP1","SOIL_TEMP2","SOIL_TEMP3"), curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
        )))
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
    saveRDS(cropped_st_fut, file = paste(data.dir,paste0("cropped_st_fut_eb_",gcm,"_",rcp),sep="/"))
    
    
    # Createing wf stars objects ####
    
    wf_hist_filelist <- hist_filelist[grep("wf", hist_filelist)]
    wf_fut_filelist <- fut_filelist[grep("wf", fut_filelist)]
    
    
    # HISTORICAL
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(wf_hist_filelist)){
      invisible(capture.output(
        suppressWarnings(
          l[[i]] <- read_stars(wf_hist_filelist[i], sub = c("PRCP","EVAP","SNOW_MELT"), curvilinear = c("longitude", "latitude")) 
        )))
    }
    # Warnings have been suppressed because they do not impact the numerical results. See here:
    # https://gis.stackexchange.com/questions/379890/decreasing-resolution-of-netcdf-data-with-gdal
    
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
    saveRDS(cropped_st_hist, file = paste(data.dir,paste0("cropped_st_hist_wf_",gcm,"_",rcp),sep="/"))
    
    # FUTURE
    
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(wf_fut_filelist)){
      invisible(capture.output(
        suppressWarnings(
          l[[i]] <- read_stars(wf_fut_filelist[i], sub = c("PRCP","EVAP","SNOW_MELT"), curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
        )))
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
    saveRDS(cropped_st_fut, file = paste(data.dir,paste0("cropped_st_fut_wf_",gcm,"_",rcp),sep="/"))
    
    
    # Createing ws stars objects ####
    
    ws_hist_filelist <- hist_filelist[grep("ws", hist_filelist)]
    ws_fut_filelist <- fut_filelist[grep("ws", fut_filelist)]
    
    
    # HISTORICAL
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(ws_hist_filelist)){
      invisible(capture.output(
        suppressWarnings(
          l[[i]] <- read_stars(ws_hist_filelist[i], sub = c("SWE","SM1","SM2","SM3"), curvilinear = c("longitude", "latitude")) 
        )))
    }
    # Warnings have been suppressed because they do not impact the numerical results. See here:
    # https://gis.stackexchange.com/questions/379890/decreasing-resolution-of-netcdf-data-with-gdal
    
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
    saveRDS(cropped_st_hist, file = paste(data.dir,paste0("cropped_st_hist_ws_",gcm,"_",rcp),sep="/"))
    
    # FUTURE
    
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(ws_fut_filelist)){
      invisible(capture.output(
        suppressWarnings(
          l[[i]] <- read_stars(ws_fut_filelist[i], sub = c("SWE","SM1","SM2","SM3"), curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
        )))
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
    saveRDS(cropped_st_fut, file = paste(data.dir,paste0("cropped_st_fut_ws_",gcm,"_",rcp),sep="/"))
    
}


# DAYMET ----
print("extracting Daymet")

# model.dir <- paste0(data.dir,"/", "Daymet")
# dir.create(model.dir,showWarnings=FALSE)

grid_filelist = list.files(path = paste(vic.dir,"monthly/daymet",sep='/'), pattern= '.nc', full.names = TRUE)

# Createing eb stars objects ####
eb_grid_filelist <-grid_filelist[grep("eb", grid_filelist)]

l <- list() # Create a list to put the stars objects into

for(i in 1:length(eb_grid_filelist)){
  invisible(capture.output(
    suppressWarnings(
      l[[i]] <- read_stars(eb_grid_filelist[i], sub = c("SOIL_TEMP1","SOIL_TEMP2","SOIL_TEMP3"), curvilinear = c("longitude", "latitude")) 
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
saveRDS(cropped_st_grid, file = paste(data.dir,"cropped_st_Daymet_eb",sep="/"))

# Createing wf stars objects ####
wf_grid_filelist <-grid_filelist[grep("wf", grid_filelist)]

l <- list() # Create a list to put the stars objects into

for(i in 1:length(wf_grid_filelist)){
  invisible(capture.output(
    suppressWarnings(
      l[[i]] <- read_stars(wf_grid_filelist[i], sub = c("PRCP","EVAP"), curvilinear = c("longitude", "latitude")) 
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
saveRDS(cropped_st_grid, file = paste(data.dir,"cropped_st_Daymet_wf",sep="/"))


# Createing ws stars objects ####
ws_grid_filelist <-grid_filelist[grep("ws", grid_filelist)]

l <- list() # Create a list to put the stars objects into

for(i in 1:length(ws_grid_filelist)){
  invisible(capture.output(
    suppressWarnings(
      l[[i]] <- read_stars(ws_grid_filelist[i], sub = c("SWE","SM1","SM2","SM3"), curvilinear = c("longitude", "latitude")) 
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
saveRDS(cropped_st_grid, file = paste(data.dir,"cropped_st_Daymet_ws",sep="/"))


rm(cropped_st_grid,cropped_st_fut,cropped_fut,cropped_grid,cropped_hist,nc_crop,nc,l,s,eb_grid_filelist,eb_fut_filelist,
   eb_hist_filelist,wf_hist_filelist,wf_fut_filelist,wf_grid_filelist,ws_hist_filelist,ws_fut_filelist,ws_grid_filelist)

  