for (G in 1:length(GCMs)){
  gcm = GCMs[G]
  for(R in 1:length(RCPs)){
    rcp = RCPs[R]
    path = paste(vic.dir, gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
    hist_filelist = Filter(function(x) grepl(paste(historical.period, collapse = "|"), x), file.list)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    
    # model.dir <- paste0(plot.dir,"/",gcm,".",rcp)
    
    # index for df
    GR <- paste(gcm,rcp,sep=".")
    index <- match(GR, GCM.RCP)
    print(c(index, GR))
  
    ############################################################################
    ##    WATER BALANCE -- CREATE STARS OBJECTS FOR T1 PLOTS   #################
    ############################################################################
    
    # shp = whatever shapefile you want to use to crop the area of interest
    
    ##### WF
    wf_hist_filelist <- hist_filelist[grep("wf", hist_filelist)]
    wf_fut_filelist <- fut_filelist[grep("wf", fut_filelist)]
    
    
    # HISTORICAL
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(wf_hist_filelist)){
      invisible(capture.output(
      suppressWarnings(
      l[[i]] <- read_stars(wf_hist_filelist[i], sub = c("PRCP","EVAP"), curvilinear = c("longitude", "latitude")) 
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
    
    # FUTURE
    
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(wf_fut_filelist)){
      invisible(capture.output(
      suppressWarnings(
      l[[i]] <- read_stars(wf_fut_filelist[i], sub = c("PRCP","EVAP"), curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
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
    
    ############################################################################
    ##    CREATE T1 variables   ################################################
    ############################################################################
    
    #Water Balance
    
    var = "Water Balance (in)"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s %>% mutate(water.balance = PRCP - EVAP) -> s 
      s = select(s, water.balance)
      if (is.na(summary(s$water.balance)[4])) {
        hist_var[[H]] = hist_var[[H-1]]
        st_dimensions(hist_var[[H]])[3] = st_dimensions(s)[3]
      } else{
        hist_var[[H]] = s[,,,] #all months
      }
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s %>% mutate(water.balance = PRCP - EVAP) -> s 
      s = select(s, water.balance)
      if (is.na(summary(s$water.balance)[4])) {
        fut_var[[F]] = fut_var[[F-1]]
        st_dimensions(fut_var[[F]])[3] = st_dimensions(s)[3]
      } else{
        fut_var[[F]] = s[,,,] #all months
      }
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(water.balancef = water.balance / 25.4) %>% select(water.balancef) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(water.balancef = water.balance / 25.4) %>% select(water.balancef) -> fut_var_stars
    
    sum_hist <- st_apply(hist_var_stars, c("x", "y"), FUN = function(x) sum(x)/length(cropped_st_hist)) # find mean
    sum_fut <- st_apply(fut_var_stars, c("x", "y"), FUN = function(x) sum(x)/length(cropped_st_fut))
    delta <- sum_fut - sum_hist
    
    
    #### Add values to Means dfs
    Baseline_Means$water_balance[index] = mean(sum_hist$water.balancef, na.rm=TRUE)
    Future_Means$water_balance[index] = mean(sum_fut$water.balancef, na.rm=TRUE)
    Deltas$water_balance[index] = mean(delta$water.balancef, na.rm=TRUE)
    
    
    # # ggplot - delta
    # ggplot() + 
    #   geom_stars(data = delta, alpha = 0.8) + 
    #   geom_sf(data = shp, aes(), fill = NA) + 
    #   scale_fill_viridis(direction=-1, option = "G",begin = .5, end = 1, 
    #                      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #mako for WB delta
    #   labs(title = paste0("Change in ", var, " -- ", gcm, ".", rcp), fill="mean (F)") +
    #   theme(legend.position = "bottom",
    #         legend.key.width = unit(2, "cm"),
    #         legend.key.height = unit(.2, "cm"),
    #         plot.title=element_text(size=12,face="bold",hjust=0.5))
    # 
    # ggsave(paste(var, gcm, rcp, ".png", sep = '_'),path = model.dir, width = 4.5, height=4)
    # 
    rm(hist_var, fut_var, hist_var_stars, fut_var_stars, sum_hist, sum_fut, delta)
    rm(cropped_fut, cropped_hist, cropped_st_fut, cropped_st_hist,l,nc,nc_crop,s,wf_hist_filelist,wf_fut_filelist)
    gc()
    
    
    
    ############################################################################
    ##    SWE -- CREATE STARS OBJECTS FOR T1 PLOTS   ###########################
    ############################################################################
    
    # shp = whatever shapefile you want to use to crop the area of interest
    
    ##### WS
    ws_hist_filelist <- hist_filelist[grep("ws", hist_filelist)]
    ws_fut_filelist <- fut_filelist[grep("ws", fut_filelist)]
    
    
    # HISTORICAL
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(ws_hist_filelist)){
      invisible(capture.output(
      suppressWarnings(
        l[[i]] <- read_stars(ws_hist_filelist[i],sub=c("SWE"), curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
      )))
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
    
    for(i in 1:length(ws_fut_filelist)){
      invisible(capture.output(
      suppressWarnings(
        l[[i]] <- read_stars(ws_fut_filelist[i], sub = c("SWE"), curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
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
    
    ############################################################################
    ##    CREATE T1 variables   ################################################
    ############################################################################
    
    #Max SWE
    
    var = "Max SWE (in)"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, SWE)
      if (is.na(summary(s$SWE)[4])) {
        hist_var[[H]] = hist_var[[H-1]]
        st_dimensions(hist_var[[H]])[3] = st_dimensions(s)[3]
      } else{
      hist_var[[H]] = s[,,,] #all months
      }
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, SWE)
      if (is.na(summary(s$SWE)[4])) {
        fut_var[[F]] = fut_var[[F-1]]
        st_dimensions(fut_var[[F]])[3] = st_dimensions(s)[3]
      } else{
        fut_var[[F]] = s[,,,] #all months
      }
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(SWEf = SWE / 25.4) %>% select(SWEf) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(SWEf = SWE / 25.4) %>% select(SWEf) -> fut_var_stars
    
  
    
    sum_hist <- st_apply(hist_var_stars, c("x", "y"), max) # find max
    sum_fut <- st_apply(fut_var_stars, c("x", "y"), max)
    
    delta <- sum_fut - sum_hist
    
    
    #### Add values to Means dfs
    Baseline_Means$Annual_max_SWE[index] = mean(sum_hist$max, na.rm=TRUE)
    Future_Means$Annual_max_SWE[index] = mean(sum_fut$max, na.rm=TRUE)
    Deltas$Annual_max_SWE[index] = mean(delta$max, na.rm=TRUE)
    
    
    # # ggplot - delta
    # ggplot() + 
    #   geom_stars(data = delta, alpha = 0.8) + 
    #   geom_sf(data = shp, aes(), fill = NA) + 
    #   scale_fill_viridis(direction=1, option = "G",begin = .5, end = 1, 
    #                      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # mako for snow
    #   labs(title = paste0("Change in ", var, " -- ", gcm, ".", rcp), fill="inches/year") +
    #   theme(legend.position = "bottom",
    #         legend.key.width = unit(2, "cm"),
    #         legend.key.height = unit(.2, "cm"),
    #         plot.title=element_text(size=12,face="bold",hjust=0.5))
    # 
    # ggsave(paste(var, gcm, rcp, ".png", sep = '_'),path = model.dir, width = 4.5, height=4)
    
    
    var = "MAM-SON SWE (in)"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, SWE)
      if (is.na(summary(s$SWE)[4])) {
        hist_var[[H]] = hist_var[[H-1]]
        st_dimensions(hist_var[[H]])[3] = st_dimensions(s[,,,c(3:5,9:11)])[3]
      } else {
        hist_var[[H]] = s[,,,c(3:5,9:11)] #all months
      }
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, SWE)
      if (is.na(summary(s$SWE)[4])) {
        fut_var[[F]] = fut_var[[F-1]]
        st_dimensions(fut_var[[F]])[3] = st_dimensions(s[,,,c(3:5,9:11)])[3]
      } else {
        fut_var[[F]] = s[,,,c(3:5,9:11)] #all months
      }
    }
      
      hist_var_stars <- Reduce(c, hist_var)
      hist_var_stars %>% mutate(SWEf = SWE / 25.4) %>% select(SWEf) -> hist_var_stars
      
      fut_var_stars <- Reduce(c, fut_var) 
      fut_var_stars %>% mutate(SWEf = SWE / 25.4) %>% select(SWEf) -> fut_var_stars
      
      
      mean_hist <- st_apply(hist_var_stars, c("x", "y"),FUN = function(x) mean(x)) # find max
      mean_fut <- st_apply(fut_var_stars, c("x", "y"), FUN = function(x) mean(x))
      
      delta <- mean_fut - mean_hist
      
      #### Add values to Means dfs
      Baseline_Means$MAM_SON_SWE[index] = mean(mean_hist$SWEf, na.rm=TRUE)
      Future_Means$MAM_SON_SWE[index] = mean(mean_fut$SWEf, na.rm=TRUE)
      Deltas$MAM_SON_SWE[index] = mean(delta$SWEf, na.rm=TRUE)
      
      # # ggplot - delta
      # ggplot() + 
      #   geom_stars(data = delta, alpha = 0.8) + 
      #   geom_sf(data = shp, aes(), fill = NA) + 
      #   scale_fill_viridis(direction=1, option = "G",begin = .5, end = 1, 
      #                      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # mako for snow
      #   labs(title = paste0("Change in ", var, " -- ", gcm, ".", rcp), fill="inches/year") +
      #   theme(legend.position = "bottom",
      #         legend.key.width = unit(2, "cm"),
      #         legend.key.height = unit(.2, "cm"),
      #         plot.title=element_text(size=12,face="bold",hjust=0.5))
      # 
      # ggsave(paste(var, gcm, rcp, ".png", sep = '_'),path = model.dir, width = 4.5, height=4)
    
    rm(hist_var, fut_var, hist_var_stars, fut_var_stars, sum_hist, sum_fut, delta)
    rm(cropped_fut, cropped_hist, cropped_st_fut, cropped_st_hist,l,nc,nc_crop,s)
    gc()
    
  }
}

  