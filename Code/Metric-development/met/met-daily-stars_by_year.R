## MET MONTHLY
DF.hist <- data.frame()
# for (G in 1:length(GCMs)){
G = 1
  # setting variables ----
  gcm = sub("\\..*", "", GCMs[G])
    rcp = sub('.*\\.', '', GCMs[G])
    path = paste(met.dir, gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc4', full.names = TRUE)
    hist_filelist = Filter(function(x) grepl(paste(historical.period, collapse = "|"), x), file.list)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    
    model.dir <- paste0(data.dir,"/",gcm,".",rcp)
    # dir.create(model.dir,showWarnings=FALSE)
    
    print(paste0("extracting ", GCMs[G]))

    # Creating stars objects ####
    
      # HISTORICAL ----
    hist_annual <- list() # Create a list to put the stars objects into
    for(i in 1:2){
      # suppressMessages(
      yr = as.Date(sub('.*\\met_', '', sub("\\..*", "", hist_filelist[i])),format="%Y")
      print(yr)
      hist_star = read_ncdf(hist_filelist[i], curvilinear = c("longitude", "latitude")) 
      hist_star = st_transform(hist_star, st_crs(shp))
      hist_crop = hist_star[shp]
      hist_crop = drop_units(hist_crop)
      
      # add Imperial units
      hist_crop %>% mutate(tmax_f = tmax * 9/5 + 32) %>%
        mutate(tmin_f = tmin * 9/5 + 32) %>% 
        mutate(pcp_in = pcp / 25.4) -> hist_crop
    
      # add threshold var
      hist_crop %>% mutate(freeze.thaw = tmax_f > 34 & tmin_f < 28) %>%
        mutate(GDD = (tmax_f + tmin_f)/2 - 0 ) %>%
        mutate(under32 = tmin < 0) %>%
        mutate(month = as.numeric(format(st_get_dimension_values(hist_crop, 'time'),"%m"))) -> hist_threshold
        # mutate(hot.pctl = quantile(tmax_f,.99,na.rm=TRUE)) 

      freeze.thaw.sum <- st_apply((hist_threshold %>% select(freeze.thaw)), c("x", "y"), sum, rename=FALSE) 
      GDD.sum <- st_apply((hist_threshold %>% select(GDD)), c("x", "y"), sum,rename=FALSE)
      under32.sum <- st_apply((hist_threshold %>% select(under32)), c("x", "y"), sum,rename=FALSE)
      
      # Don't know how to add/retain time dimension for year
      
      by_t = "1 month"
      under32.month = aggregate((hist_threshold %>% select(under32)), by = by_t, FUN = sum)
      # under32.split <- split(under32.month, "time")
      
      WSF.below32 <- st_apply(under32.month[,c(1:5,9:12)],c("x", "y"),sum,rename=FALSE)
      names(WSF.below32) <- "WSF.below32"
      W.under32 <- st_apply(under32.month[,c(1:2,12)],c("x", "y"),sum,rename=FALSE)
      names(W.under32) <- "W.under32"
      
      
      hist_annual[[i]]<- annual_thresholds <- c(freeze.thaw.sum, GDD.sum,under32.sum,WSF.below32,W.under32)
      
      ## timeseries df
      df<-data.frame(GCM = GCMs[G],year = yr)
      s <- st_apply(annual_thresholds,1:2,mean)
      df$freeze.thaw = mean(s$freeze.thaw,na.rm=TRUE)
      df$GDD = mean(s$GDD,na.rm=TRUE)
      df$under32 = mean(s$under32,na.rm=TRUE)
      df$WSF.below32 = mean(s$WSF.below32,na.rm=TRUE)
      df$W.under32 = mean(s$W.under32,na.rm=TRUE)

      DF.hist <- rbind(DF.hist,df)
      
      rm(hist_star,hist_crop,hist_threshold,freeze.thaw.sum, GDD.sum, under32.sum, under32.month, WSF.below32,
         w.under32,s)
      gc()
      # )
    }
    
    
    ##### TESTING COMBINING STARS LIST
    aggregate(hist_annual,by=,c("x", "y"),FUN=mean)
    Hist_annual <- Reduce(c,hist_annual)
    
    Hist_annual <- st_apply(hist_annual,c("x", "y"),mean)
    
    H <- data.frame(hist_annual)
    H_sf<-sf::st_as_sf(H,coords = c("x", "y"), crs = 3338)
    # plot(st_geometry(H_sf))
    # plot(st_geometry(H_sf$GDD))
    plot(H_sf) #works
    H_aggregate_ft <- H_sf %>%
      group_by(geometry) %>% summarise(freeze.thaw,mean)
    
    
      
      
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
    saveRDS(cropped_st_hist, file = paste(model.dir,paste0("cropped_st_hist_daily_",gcm,"_",rcp),sep="/"))
    
    
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
    saveRDS(cropped_st_fut, file = paste(model.dir,paste0("cropped_st_fut_daily_",gcm,"_",rcp),sep="/"))
}
print("extracting Daymet")

model.dir <- paste0(data.dir,"/", "Daymet")
# dir.create(model.dir,showWarnings=FALSE)

grid_filelist = list.files(path = paste(met.dir,"daymet",sep='/'), pattern= '.nc', full.names = TRUE)

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
saveRDS(cropped_st_grid, file = paste(model.dir,"cropped_st_Daymet_daily_",sep="/"))


rm(cropped_st_grid,cropped_st_fut,cropped_fut,cropped_grid,cropped_hist,nc_crop,nc,l,nc,s)

  