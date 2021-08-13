# Annual maxSWE ----
var = "max.SWE"
DF = data.frame()

cropped_st_grid <- readRDS(paste(data.dir,"cropped_st_Daymet_ws",sep="/"))

grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, SWE)
  if (is.na(summary(s$SWE)[4])) {
    grid_var[[F]] = grid_var[[F-1]]
    st_dimensions(grid_var[[F]])[3] = st_dimensions(s)[3]
  } else{
    grid_var[[F]] = s[,,,] #all months
  }
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$SWE <- drop_units(grid_var_stars$SWE)
grid_var_stars %>% mutate(SWEf = SWE / 25.4) %>% select(SWEf) -> grid_var_stars

# st_get_dimension_values(grid_var_stars,"time") #how get time dimension values
by_t = "1 year"
grid <- aggregate(grid_var_stars, by = by_t, FUN = function(x) max(x)) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
grid1 <- split(grid, "time")


df<-data.frame(year=daymet.period,mean=NA)
for (i in 1:length(daymet.period)){
  t <-st_apply(grid1[i],1:2,mean)
  df$mean[i] <- mean(t$mean,na.rm=TRUE)
}
df$GCM <- "Daymet"; names(df) <- c("Year", var, "GCM")
DF <- rbind(DF,df)

# mean_grid <- st_apply(grid, c("x", "y"), mean)
# saveRDS(mean_grid, file = paste0(data.dir,"/",var,gcm))

for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  # cf = CF_GCM$CF[match(gcm, CF_GCM$GCM)]
  # stars objs
  cropped_st_hist <- readRDS(paste(data.dir,paste0("cropped_st_hist_ws_",gcm,"_",rcp),sep="/"))
  cropped_st_fut <- readRDS(paste(data.dir,paste0("cropped_st_fut_ws_",gcm,"_",rcp),sep="/"))

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
  hist_var_stars$SWE <- drop_units(hist_var_stars$SWE)
  hist_var_stars %>% mutate(SWEf = SWE / 25.4) %>% select(SWEf) -> hist_var_stars
  
  fut_var_stars <- Reduce(c, fut_var)
  fut_var_stars$SWE <- drop_units(fut_var_stars$SWE)
  fut_var_stars %>% mutate(SWEf = SWE / 25.4) %>% select(SWEf) -> fut_var_stars

  by_t = "1 year"
  hist <- aggregate(hist_var_stars, by = by_t, FUN = function(x) max(x)) #Don't need to divide by #yrs b/c by year
  hist <- hist[,2:51,,]
  # hist1 <- split(hist, "time")
  # 
  # 
  # df<-data.frame(year=historical.period,mean=NA)
  # for (i in 1:length(historical.period)){
  #   t <-st_apply(hist1[i],1:2,mean)
  #   df$mean[i] <- mean(t$mean,na.rm=TRUE)
  # }
  # df$GCM <- GCMs[G]; names(df) <- c("Year", var, "GCM")
  # DF.hist<-rbind(DF.hist,df)

  
  fut <- aggregate(fut_var_stars, by = by_t, FUN = function(x) max(x)) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
  fut <- fut[,2:32,,]
  fut1 <- split(fut, "time")
  
  df<-data.frame(year=future.period,mean=NA)
  for (i in 1:length(future.period)){
    t <-st_apply(fut1[i],1:2,mean)
    df$mean[i] <- mean(t$mean,na.rm=TRUE)
  }
  df$GCM <- GCMs[G]; names(df) <- c("Year", var, "GCM")
  DF<-rbind(DF,df)

  
  mean_hist <- st_apply(hist, c("x", "y"), mean) # find mean
  mean_fut <- st_apply(fut, c("x", "y"), mean)
  delta <- mean_fut - mean_hist
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))

}

write.csv(DF,paste0(data.dir,"/",var,"_ANN.csv"),row.names=FALSE)

rm(grid_var,grid_var_stars,grid,grid1,hist,hist_var,hist_var_stars,mean_hist,fut,fut1,fut_var,fut_var_stars,mean_fut)
gc()
