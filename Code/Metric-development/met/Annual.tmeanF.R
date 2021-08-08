# Annual Tmean ----
var = "Annual.tmeanF"
DF.hist <- data.frame()
DF.fut <- data.frame()


for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  # cf = CF_GCM$CF[match(gcm, CF_GCM$GCM)]
  model.dir <- paste0(data.dir,"/",GCMs[G])
  # stars objs
  cropped_st_hist <- readRDS(paste(model.dir,paste0("cropped_st_hist_",gcm,"_",rcp),sep="/"))
  cropped_st_fut <- readRDS(paste(model.dir,paste0("cropped_st_fut_",gcm,"_",rcp),sep="/"))
  # assign(cropped_st_hist,paste0("cropped_st_hist_",GCMs[G]))
  # assign(cropped_st_fut,paste0("cropped_st_fut_",GCMs[G]))
  
  hist_var <- list()
  
  for(H in 1:length(cropped_st_hist)){
    s = cropped_st_hist[[H]]
    s %>% mutate(tmean = (tmax + tmin)/2) -> s 
    s = select(s, tmean)
    hist_var[[H]] = s[,,,] #all months
  }
  
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s %>% mutate(tmean = (tmax + tmin)/2) -> s 
    s = select(s, tmean)
    fut_var[[F]] = s[,,,] #all months
  }
  
  hist_var_stars <- Reduce(c, hist_var)
  hist_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> hist_var_stars
  
  fut_var_stars <- Reduce(c, fut_var) 
  fut_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> fut_var_stars
  
  by_t = "1 year"
  hist <- aggregate(hist_var_stars, by = by_t, FUN = mean, na.omit = TRUE) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
  hist <- hist[,2:51,,]
  hist1 <- split(hist, "time")
  
  
  df<-data.frame(year=historical.period,mean=NA)
  for (i in 1:length(historical.period)){
    t <-st_apply(hist1[i],1:2,mean)
    df$mean[i] <- mean(t$mean,na.rm=TRUE)
  }
  df$GCM <- GCMs[G]; names(df) <- c("Year", var, "GCM")
  DF.hist<-rbind(DF.hist,df)

  
  fut <- aggregate(fut_var_stars, by = by_t, FUN = mean, na.omit = TRUE) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
  fut <- fut[,2:31,,]
  fut1 <- split(fut, "time")
  
  df<-data.frame(year=future.period,mean=NA)
  for (i in 1:length(future.period)){
    t <-st_apply(fut1[i],1:2,mean)
    df$mean[i] <- mean(t$mean,na.rm=TRUE)
  }
  df$GCM <- GCMs[G]; names(df) <- c("Year", var, "GCM")
  DF.fut<-rbind(DF.fut,df)

  
  mean_hist <- st_apply(hist, c("x", "y"), mean) # find mean
  mean_fut <- st_apply(fut, c("x", "y"), mean)
  delta <- mean_fut - mean_hist
  saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))

}
# Baseline_Annual <- merge(Baseline_Annual,DF.hist,by=c("GCM","Year"),all=TRUE)
# Future_Annual <- merge(Future_Annual,DF.fut ,by=c("GCM","Year"),all=TRUE)

write.csv(DF.hist,paste0(data.dir,"/Annual_hist",var,".csv"),row.names=FALSE)
write.csv(DF.fut,paste0(data.dir,"/Annual_fut",var,".csv"),row.names=FALSE)

model.dir <- paste0(data.dir,"/", "Daymet")
cropped_st_grid <- readRDS(paste(model.dir,"cropped_st_Daymet",sep="/"))

grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s 
  s = select(s, tmean)
  grid_var[[F]] = s[,,,] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmean <- drop_units(grid_var_stars$tmean)
grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> grid_var_stars

# st_get_dimension_values(grid_var_stars,"time") #how get time dimension values

grid <- aggregate(grid_var_stars, by = by_t, FUN = mean, na.omit = TRUE) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
grid <- grid[,2:38,,]
grid1 <- split(grid, "time")


df<-data.frame(year=daymet.period,mean=NA)
for (i in 1:length(daymet.period)){
t <-st_apply(grid1[i],1:2,mean)
df$mean[i] <- mean(t$mean,na.rm=TRUE)
}
df$GCM <- "Daymet"; names(df) <- c("Year", var, "GCM")
# Daymet_Annual <- merge(Daymet_Annual,df,by=c("GCM","Year"),all=TRUE)

# test2 <- split(test, "time") # ggplot will not work if time is a dimension, so switching to an attribute. Should not matter since time is aggregated here. 
mean_grid <- st_apply(grid, c("x", "y"), mean)
saveRDS(mean_grid, file = paste0(model.dir,"/",var,gcm))
write.csv(df,paste0(data.dir,"/Annual_daymet",var,".csv"),row.names=FALSE)

rm(grid_var,grid_var_stars,grid,grid1,mean_grid,hist,hist1,hist_var,hist_var_stars,mean_hist,fut,fut1,fut_var,fut_var_stars,mean_fut)
gc()
