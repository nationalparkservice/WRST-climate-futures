# Run freeze.thaw, GDD, under32, ,WSF.below32, w.days.over32, and freq.hot.days to avoid reading in data multiple times

DF.hist <- data.frame()
DF.fut <- data.frame()

for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  # cf = CF_GCM$CF[match(gcm, CF_GCM$GCM)]
  model.dir <- paste0(data.dir,"/",GCMs[G])
  # stars objs
  cropped_st_hist <- readRDS(paste(model.dir,paste0("cropped_st_hist_daily_",gcm,"_",rcp),sep="/"))
 
  # Annual freeze thaw ----
  var = "freeze.thaw"
  
  hist_var <- list()
  
  for(H in 1:length(cropped_st_hist)){
    s = cropped_st_hist[[H]]
    s = select(s, tmin,tmax)
    
    hist_var[[H]] = s[,,,] #all months
  }
  rm(cropped_st_hist)
  
  hist_var_stars <- Reduce(c, hist_var)
  hist_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% 
    %>%  mutate(tmin_f = tmin * 9/5 + 32) %>%
    
  
    select(tax_f) -> hist_var_stars
  
  
  by_t = "1 year"
  hist <- aggregate(hist_var_stars, by = by_t, FUN = function(x) sum(x) *30) #Don't need to divide by #yrs b/c by year
  hist1 <- split(hist, "time")
  
  
  df<-data.frame(year=historical.period,mean=NA)
  for (i in 1:length(historical.period)){
    t <-st_apply(hist1[i],1:2,mean)
    df$mean[i] <- mean(t$mean,na.rm=TRUE)
  }
  df$GCM <- GCMs[G]; names(df) <- c("Year", var, "GCM")
  DF.hist<-rbind(DF.hist,df)
  
  mean_hist <- st_apply(hist, c("x", "y"), mean) # find mean
  
  
  
  cropped_st_fut <- readRDS(paste(model.dir,paste0("cropped_st_fut_daily_",gcm,"_",rcp),sep="/"))
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s = select(s, tmin,tmax)
    fut_var[[F]] = s[,,,] #all months
  }
  
 

  fut_var_stars <- Reduce(c, fut_var)
  fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_stars


  
  fut <- aggregate(fut_var_stars, by = by_t, FUN = function(x) sum(x) *30) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
  fut1 <- split(fut, "time")
  
  df<-data.frame(year=future.period,mean=NA)
  for (i in 1:length(future.period)){
    t <-st_apply(fut1[i],1:2,mean)
    df$mean[i] <- mean(t$mean,na.rm=TRUE)
  }
  df$GCM <- GCMs[G]; names(df) <- c("Year", var, "GCM")
  DF.fut<-rbind(DF.fut,df)

  
  mean_fut <- st_apply(fut, c("x", "y"), mean)
  delta <- mean_fut - mean_hist
  saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))

}
Baseline_Annual <- merge(Baseline_Annual,DF.hist,by=c("GCM","Year"),all=TRUE)
Future_Annual <- merge(Future_Annual,DF.fut ,by=c("GCM","Year"),all=TRUE)

model.dir <- paste0(data.dir,"/", "Daymet")
cropped_st_grid <- readRDS(paste(model.dir,"cropped_st_Daymet",sep="/"))

grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, pcp)
  grid_var[[F]] = s[,,,] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$pcp <- drop_units(grid_var_stars$pcp)
grid_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> grid_var_stars

# st_get_dimension_values(grid_var_stars,"time") #how get time dimension values

grid <- aggregate(grid_var_stars, by = by_t, FUN = function(x) sum(x) *30) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
grid1 <- split(grid, "time")


df<-data.frame(year=daymet.period,mean=NA)
for (i in 1:length(daymet.period)){
t <-st_apply(grid1[i],1:2,mean)
df$mean[i] <- mean(t$mean,na.rm=TRUE)
}
df$GCM <- "Daymet"; names(df) <- c("Year", var, "GCM")
Daymet_Annual <- merge(Daymet_Annual,df,by=c("GCM","Year"),all=TRUE)

# test2 <- split(test, "time") # ggplot will not work if time is a dimension, so switching to an attribute. Should not matter since time is aggregated here. 
mean_grid <- st_apply(grid, c("x", "y"), mean)
saveRDS(mean_grid, file = paste0(model.dir,"/",var,gcm))

rm(grid_var,grid_var_stars,grid,grid1,mean_grid,hist,hist1,hist_var,hist_var_stars,mean_hist,fut,fut1,fut_var,fut_var_stars,mean_fut)

