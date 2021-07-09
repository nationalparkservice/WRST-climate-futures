for (G in 1:1){
  gcm = GCMs[G]
  for(R in 1:1){
    rcp = RCPs[R]
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
    
    # FUTURE
    
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
    
    ############################################################################
    ##    CREATE T1 variables   ################################################
    ############################################################################
    
    #Tmean
    
    #Precip
    
    
    # Seasonal Precip
  
  var = "MAM precip (in)"
  hist_var <- list()
  
  for(H in 1:length(cropped_st_hist)){
    s = cropped_st_hist[[H]]
    s = select(s, pcp)
    hist_var[[H]] = s[,,,3:5] #set for months
  }
  
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s = select(s, pcp)
    fut_var[[F]] = s[,,,3:5] #set for months
  }
  
  hist_var_stars <- Reduce(c, hist_var)
  hist_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_var_stars
  
  fut_var_stars <- Reduce(c, fut_var) 
  fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_stars
  
  sum_hist <- st_apply(hist_var_stars, c("x", "y"), sum) # find sum
  sum_fut <- st_apply(fut_var_stars, c("x", "y"), sum)
  delta <- sum_fut - sum_hist
  
 
  #### Add values to Means dfs
  # index for df
  index <- which(gcm %in% GCMs) * which(rcp %in% RCPs)
  
  Baseline_Means$GCM[index] = gcm
  Baseline_Means$RCP[index] = rcp
  Baseline_Means$MAM_Precip_in[index] = mean(sum_hist$sum, na.rm=TRUE)
  
  Future_Means$GCM[index] = gcm
  Future_Means$RCP[index] = rcp
  Future_Means$MAM_Precip_in[index] = mean(sum_fut$sum, na.rm=TRUE)
  
  Deltas$GCM[index] = gcm
  Deltas$RCP[index] = rcp
  Deltas$MAM_Precip_in[index] = mean(delta$sum, na.rm=TRUE)

  
  # ggplot - delta
ggplot() + 
    geom_stars(data = delta, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    scale_fill_viridis(direction=-1, option = "C",begin = .5, end = 1, 
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #plasma for precip delta
    labs(title = paste0("Change in ", var, " -- ", gcm, ".", rcp), fill="inches/year") +
        theme(legend.position = "bottom",
              legend.key.width = unit(2, "cm"),
              legend.key.height = unit(.2, "cm"),
          plot.title=element_text(size=12,face="bold",hjust=0.5))

  ggsave(paste(var, gcm, rcp, ".png", sep = '_'),path = plot.dir, width = 4.5, height=4)
  
  rm(hist_var, fut_var, hist_var_stars, fut_var_stars, sum_hist, sum_fut, delta)
  }
}
  