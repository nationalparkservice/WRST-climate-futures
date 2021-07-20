## MET MONTHLY
# for (G in 1:length(GCMs)){
for (G in 1:1){
  gcm = sub("\\..*", "", GCMs[G])
    rcp = sub('.*\\.', '', GCMs[G])
    path = paste(data.dir,"monthly/BCSD", gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
    hist_filelist = Filter(function(x) grepl(paste(historical.period, collapse = "|"), x), file.list)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    grid_filelist = list.files(path = paste(data.dir,"monthly/daymet",sep='/'), pattern= '.nc', full.names = TRUE)
    
    model.dir <- paste0(plot.dir,"/",gcm,".",rcp)
    dir.create(model.dir,showWarnings=FALSE)
    
    print(paste0("extracting ", GCMs[G]))

    ############################################################################
    ##    CREATE STARS OBJECTS #################################################
    ############################################################################
    
    ################ HISTORICAL ##############
    
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
    saveRDS(cropped_st_hist, file = paste(model.dir,paste0("cropped_st_hist_",GCMs[G]),sep="/"))
    
    
    ################ FUTURE ##############
    
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
      cropped_st_fut <- st_as_stars(cropped_fut[[i]])
    }
    # assign(paste0("cropped_st_fut_",GCMs[G]), cropped_st_fut)
    saveRDS(cropped_st_fut, file = paste(model.dir,paste0("cropped_st_fut_",GCMs[G]),sep="/"))
    
    ################ DAYMET ##############
    
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
   saveRDS(cropped_st_grid, file = paste(model.dir,paste0("cropped_st_grid_",GCMs[G]),sep="/"))
}

## MET DAILY
for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  path = paste(data.dir, gcm, rcp, sep = '/')
  file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
  hist_filelist = Filter(function(x) grepl(paste(historical.period, collapse = "|"), x), file.list)
  fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
  
  model.dir <- paste0(plot.dir,"/",gcm,".",rcp)
  dir.create(model.dir,showWarnings=FALSE)
  
  print(paste0("extracting ", GCMs[G]))
  
  ############################################################################
  ##    CREATE STARS OBJECTS #################################################
  ############################################################################
  
  ################ HISTORICAL ##############
  
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
  
  assign(paste0("cropped_st_hist_",GCMs[G]), list())
  
  for(i in 1:length(cropped_hist)){
    cropped_st_hist[[i]] <- st_as_stars(cropped_hist[[i]])
  }
  
  ################ FUTURE ##############
  
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
  
  assign(paste0("cropped_st_fut_",GCMs[G]), list())
  
  for(i in 1:length(cropped_fut)){
    eval(paste0("cropped_st_fut_",GCMs[G]))[[i]] <- st_as_stars(cropped_fut[[i]])
  }
  
  ################ GRIDMET ##############
  
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
  
  assign(paste0("cropped_st_grid_",GCMs[G]), list())
  
  for(i in 1:length(cropped_fut)){
    cropped_st_fut[[i]] <- st_as_stars(cropped_fut[[i]])
  }
  
}
  rm(hist_var, fut_var, hist_var_stars, fut_var_stars, sum_hist, sum_fut, delta,mean_hist,mean_fut)



rm(cropped_fut, cropped_hist, cropped_st_fut, cropped_st_hist,l,nc,nc_crop,s)
  