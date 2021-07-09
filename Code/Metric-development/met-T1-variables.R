# for (G in 1:length(GCMs)){
  gcm = GCMs[1]
  # for(R in 1:length(RCPs[R])){
    rcp = RCPs[1]
    path = paste(data.dir, gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
    
    hist_filelist = Filter(function(x) grepl(paste(historical.period, collapse = "|"), x), file.list)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
  
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
    
    ############################################################################
    ##    CREATE T1 variables   ################################################
    ############################################################################
    
  
  # Seasonal Precip
  
  var = "MAM precip (in)"
  hist_MAM_pcp <- list()
  
  for(H in 1:length(cropped_st_hist)){
    s = cropped_st_hist[[H]]
    s = select(s, pcp)
    hist_MAM_pcp[[H]] = s[,,,3:5]
  }
  
  fut_MAM_pcp <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s = select(s, pcp)
    fut_MAM_pcp[[F]] = s[,,,3:5]
  }
  
  hist_MAM_pcp_stars <- Reduce(c, hist_MAM_pcp)
  hist_MAM_pcp_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_MAM_pcp_stars
  
  fut_MAM_pcp_stars <- Reduce(c, fut_MAM_pcp) 
  fut_MAM_pcp_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_MAM_pcp_stars
  
  sum_hist_pcp <- st_apply(hist_MAM_pcp_stars, c("x", "y"), sum) # find sum
  sum_fut_pcp <- st_apply(fut_MAM_pcp_stars, c("x", "y"), sum)
  delta <- sum_fut_pcp - sum_hist_pcp
  
  Baseline_Means$GCM[1] = gcm
  Baseline_Means$RCP[1] = rcp
  Baseline_Means$MAM_Precip_in[1] = mean(sum_hist_pcp$sum, na.rm=TRUE)
  
  Future_Means$GCM[1] = gcm
  Future_Means$RCP[1] = rcp
  Future_Means$MAM_Precip_in = mean(sum_fut_pcp$sum, na.rm=TRUE)
  
  Delta$GCM[1] = gcm
  Delta$RCP[1] = rcp
  Delta$MAM_Precip_in = mean(delta$sum, na.rm=TRUE)

  
  # ggplot - delta
  
  ggplot() + 
    geom_stars(data = delta, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    scale_fill_viridis() + 
    theme_map() +
    theme(legend.position = "bottom") +
    theme(legend.key.width = unit(2, "cm")) 
  
  
  ggsave(filename = paste('./maps/Deltas/', GCMs[G], RCPs[R], "pcp_hist.png", sep = '_'))
  
  