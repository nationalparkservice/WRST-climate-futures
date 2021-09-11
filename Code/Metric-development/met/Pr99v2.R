## MET MONTHLY
print("extracting Daymet")
DF = data.frame()

grid_filelist = list.files(path = paste(met.dir,"daymet",sep='/'), pattern= '.nc', full.names = TRUE)

# DAYMET ----
for(i in 1:length(grid_filelist)){
# grid_annual <- list() # Create a list to put the stars objects into
yr = as.POSIXct(sub('.*\\met_', '', sub("\\..*", "", grid_filelist[i])),format="%Y")
print(yr)
invisible(capture.output(
  suppressWarnings(
    (grid_star = read_stars(grid_filelist[i], sub=c("tmax","tmin","pcp") ,curvilinear = c("longitude", "latitude"))))))
grid_star = st_transform(grid_star, st_crs(shp))
grid_crop = grid_star[shp]
grid_crop = drop_units(grid_crop)
rm(grid_star)

# add Imperial units
grid_crop %>% mutate(tmax_f = tmax * 9/5 + 32) %>%
  mutate(tmin_f = tmin * 9/5 + 32) %>% 
  mutate(pcp_in = pcp / 25.4) -> grid_crop

# add threshold var
grid_crop %>% mutate(freeze.thaw = tmax_f > 34 & tmin_f < 28) %>%
  mutate(GDD = (tmax_f + tmin_f)/2 - 0 ) %>%
  mutate(under32 = tmin < 0) %>%
  mutate(over20 = tmax > 20) %>%
  mutate(pcp.over.5 = pcp_in > 0.5) -> grid_threshold
  # mutate(month = as.numeric(format(st_get_dimension_values(grid_crop, 'time'),"%m"))) 
  # mutate(hot.pctl = quantile(tmax_f,.99,na.rm=TRUE)) 

freeze.thaw.sum <- st_apply((grid_threshold %>% dplyr::select(freeze.thaw)), c("x", "y"), sum, rename=FALSE) 
GDD.sum <- st_apply((grid_threshold %>% dplyr::select(GDD)), c("x", "y"), sum,rename=FALSE)
under32.sum <- st_apply((grid_threshold %>% dplyr::select(under32)), c("x", "y"), sum,rename=FALSE)
over20.sum <- st_apply((grid_threshold %>% dplyr::select(over20)), c("x", "y"), sum,rename=FALSE)
pcp.over.5.sum <- st_apply((grid_threshold %>% dplyr::select(pcp.over.5)), c("x", "y"), sum,rename=FALSE)
# Don't know how to add/retain time dimension for year

by_t = "1 month"
under32.month = aggregate((grid_threshold %>% dplyr::select(under32)), by = by_t, FUN = sum)
# under32.split <- split(under32.month, "time")

WSF.below32 <- st_apply(under32.month[,c(1:5,9:12)],c("x", "y"),sum,rename=FALSE)
names(WSF.below32) <- "WSF.below32"
W.under32 <- st_apply(under32.month[,c(1:2,12)],c("x", "y"),sum,rename=FALSE)
names(W.under32) <- "W.under32"

annual_thresholds <- c(freeze.thaw.sum, GDD.sum,under32.sum,over20.sum,WSF.below32,W.under32,pcp.over.5.sum)

## timeseries df
df<-data.frame(GCM = "Daymet",year = yr)
s <- st_apply(annual_thresholds,1:2,mean)
df$freeze.thaw = mean(s$freeze.thaw,na.rm=TRUE)
df$GDD = mean(s$GDD,na.rm=TRUE)
df$under32 = mean(s$under32,na.rm=TRUE)
df$over20 = mean(s$over20,na.rm=TRUE)
df$WSF.below32 = mean(s$WSF.below32,na.rm=TRUE)
df$W.under32 = mean(s$W.under32,na.rm=TRUE)
df$pcp.over.5 = mean(s$pcp.over.5,na.rm=TRUE)

DF = rbind(DF, df)
rm(annual_thresholds,freeze.thaw.sum, GDD.sum,under32.sum,over20.sum,WSF.below32,W.under32,pcp.over.5.sum,
   grid_threshold,grid_crop)
gc()
}

for (G in 1:length(GCMs)){
  # setting variables ----
  gcm = sub("\\..*", "", GCMs[G])
    rcp = sub('.*\\.', '', GCMs[G])
    path = paste(met.dir, gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc4', full.names = TRUE)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    
    # model.dir <- paste0(data.dir,"/",gcm,".",rcp)
    # dir.create(model.dir,showWarnings=FALSE)
    
    print(paste0("extracting ", GCMs[G]))

    # Creating stars objects ####
    
      # HISTORICAL ----
    # fut_annual <- list() # Create a list to put the stars objects into
    for(i in 1:length(fut_filelist)){
      yr = as.POSIXct(sub('.*\\met_', '', sub("\\..*", "", fut_filelist[i])),format="%Y")
      print(yr)
      invisible(capture.output(
        suppressWarnings(
      (fut_star = read_ncdf(fut_filelist[i], var=c("tmax","tmin","pcp"), curvilinear = c("longitude", "latitude"))))))
      fut_star = st_transform(fut_star, st_crs(shp))
      fut_crop = fut_star[shp]
      fut_crop = drop_units(fut_crop)
      rm(fut_star)
      
      # add Imperial units
      fut_crop %>% mutate(tmax_f = tmax * 9/5 + 32) %>%
        mutate(tmin_f = tmin * 9/5 + 32) %>% 
        mutate(pcp_in = pcp / 25.4) -> fut_crop
    
      # add threshold var
      fut_crop %>% mutate(freeze.thaw = tmax_f > 34 & tmin_f < 28) %>%
        mutate(GDD = (tmax_f + tmin_f)/2 - 0 ) %>%
        mutate(under32 = tmin < 0) %>%
        mutate(over20 = tmax > 20) %>%
        mutate(pcp.over.5 = pcp_in > 0.5) -> fut_threshold

      freeze.thaw.sum <- st_apply((fut_threshold %>% dplyr::select(freeze.thaw)), c("x", "y"), sum, rename=FALSE) 
      GDD.sum <- st_apply((fut_threshold %>% dplyr::select(GDD)), c("x", "y"), sum,rename=FALSE)
      under32.sum <- st_apply((fut_threshold %>% dplyr::select(under32)), c("x", "y"), sum,rename=FALSE)
      over20.sum <- st_apply((fut_threshold %>% dplyr::select(over20)), c("x", "y"), sum,rename=FALSE)
      pcp.over.5.sum <- st_apply((fut_threshold %>% dplyr::select(pcp.over.5)), c("x", "y"), sum,rename=FALSE)
      
      # Don't know how to add/retain time dimension for year
      
      by_t = "1 month"
      under32.month = aggregate((fut_threshold %>% dplyr::select(under32)), by = by_t, FUN = sum)
      # under32.split <- split(under32.month, "time")
      
      WSF.below32 <- st_apply(under32.month[,c(1:5,9:12)],c("x", "y"),sum,rename=FALSE)
      names(WSF.below32) <- "WSF.below32"
      W.under32 <- st_apply(under32.month[,c(1:2,12)],c("x", "y"),sum,rename=FALSE)
      names(W.under32) <- "W.under32"
      
      annual_thresholds <- c(freeze.thaw.sum, GDD.sum,under32.sum,over20.sum,WSF.below32,W.under32,pcp.over.5.sum )
      
      ## timeseries df
      df<-data.frame(GCM = GCMs[G],year = yr)
      s <- st_apply(annual_thresholds,1:2,mean)
      df$freeze.thaw = mean(s$freeze.thaw,na.rm=TRUE)
      df$GDD = mean(s$GDD,na.rm=TRUE)
      df$under32 = mean(s$under32,na.rm=TRUE)
      df$over20 = mean(s$over20,na.rm=TRUE)
      df$WSF.below32 = mean(s$WSF.below32,na.rm=TRUE)
      df$W.under32 = mean(s$W.under32,na.rm=TRUE)
      df$pcp.over.5 = mean(s$pcp.over.5,na.rm=TRUE)

      DF <- rbind(DF,df)
      
      rm(annual_thresholds,freeze.thaw.sum, GDD.sum,under32.sum,over20.sum,WSF.below32,W.under32,pcp.over.5.sum,
         fut_threshold,fut_crop)
      gc()
      # )
}
}

write.csv(DF,paste0(data.dir,"/Daily_met.csv"),row.names=FALSE)

