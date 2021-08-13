# Annual SWE:precip  ----
var = "SWE.precip"
DF = data.frame()

# DAYMET ----
# model.dir <- paste0(data.dir,"/", "Daymet")

  cropped_st_wf <- readRDS(paste(data.dir,"cropped_st_Daymet_wf",sep="/"))
  cropped_st_ws <- readRDS(paste(data.dir,"cropped_st_Daymet_ws",sep="/"))
  
  grid_var_wf <- list()
  
  for(F in 1:length(cropped_st_wf)){
    s = cropped_st_wf[[F]]
    s = dplyr::select(s, c(PRCP))
    if (is.na(summary(s$PRCP)[4])) {
      grid_var_wf[[F]] = grid_var_wf[[F-1]]
      st_dimensions(grid_var_wf[[F]])[3] = st_dimensions(s)[3]
    } else{
      grid_var_wf[[F]] = s[,,,] #all months
    }
  }
  
  grid_var_ws <- list()
  
  for(F in 1:length(cropped_st_ws)){
    s = cropped_st_ws[[F]]
    s = dplyr::select(s, c(SWE))
    if (is.na(summary(s$SWE)[4])) {
      grid_var_ws[[F]] = grid_var_ws[[F-1]]
      st_dimensions(grid_var_ws[[F]])[3] = st_dimensions(s)[3]
    } else{
      grid_var_ws[[F]] = s[,,,] #all months
    }
  }
  
  wf_var <- Reduce(c, grid_var_wf)
  wf_var <-drop_units(wf_var)
  wf_var %>% dplyr::select(PRCP) -> var_prcp
  
  ws_var <- Reduce(c, grid_var_ws)
  ws_var <-drop_units(ws_var)
  ws_var %>% dplyr::select(SWE) -> var_swe
  
  by_t = "1 year"
  p <- aggregate(var_prcp, by = by_t, FUN = function(x) sum(x)) #Don't need to divide by #yrs b/c by year
  p1 <- split(p, "time")
  s <- aggregate(var_swe, by = by_t, FUN = function(x) max(x)) #Don't need to divide by #yrs b/c by year
  s1 <- split(s, "time")
  
  ratio1 <- s1[2:38,,]/p1[2:38,,]
  ratio  <- s[,2:38,,]/p[,2:38,,]

  
  df<-data.frame(year=daymet.period,mean=NA)
  for (i in 1:length(daymet.period)){
    t <-st_apply(ratio1[i],1:2,mean)
    df$mean[i] <- mean(t$mean,na.rm=TRUE)
  }
  df$GCM <- "Daymet"; names(df) <- c("Year", var, "GCM")
  DF <- rbind(DF,df)
  
  mean_grid<-st_apply(ratio, c("x", "y"), mean)
  # saveRDS(r, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
  rm(ratio,ratio1,s,s1,p,p1,ws_var,wf_var,grid_var_ws,grid_var_wf,cropped_st_wf,cropped_st_ws)
  gc()

# FUTURE ----
for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  # cf = CF_GCM$CF[match(gcm, CF_GCM$GCM)]
  # model.dir <- paste0(data.dir,"/",GCMs[G])
  # stars objs
  cropped_st_wf <- readRDS(paste(data.dir,paste0("cropped_st_fut_wf_",gcm,"_",rcp),sep="/"))
  cropped_st_ws <- readRDS(paste(data.dir,paste0("cropped_st_fut_ws_",gcm,"_",rcp),sep="/"))

  fut_var_wf <- list()
  
  for(F in 1:length(cropped_st_wf)){
    s = cropped_st_wf[[F]]
    s = dplyr::select(s, c(PRCP))
    if (is.na(summary(s$PRCP)[4])) {
      fut_var_wf[[F]] = fut_var_wf[[F-1]]
      st_dimensions(fut_var_wf[[F]])[3] = st_dimensions(s)[3]
    } else{
      fut_var_wf[[F]] = s[,,,] #all months
    }
  }
  
  fut_var_ws <- list()
  
  for(F in 1:length(cropped_st_ws)){
    s = cropped_st_ws[[F]]
    s = dplyr::select(s, c(SWE))
    if (is.na(summary(s$SWE)[4])) {
      fut_var_ws[[F]] = fut_var_ws[[F-1]]
      st_dimensions(fut_var_ws[[F]])[3] = st_dimensions(s)[3]
    } else{
      fut_var_ws[[F]] = s[,,,] #all months
    }
  }
  
  wf_var <- Reduce(c, fut_var_wf)
  wf_var <-drop_units(wf_var)
  wf_var %>% dplyr::select(PRCP) -> var_prcp

  ws_var <- Reduce(c, fut_var_ws)
  ws_var <-drop_units(ws_var)
  ws_var %>% dplyr::select(SWE) -> var_swe

  by_t = "1 year"
  p <- aggregate(var_prcp, by = by_t, FUN = function(x) sum(x)) #Don't need to divide by #yrs b/c by year
  p1 <- split(p, "time")
  s <- aggregate(var_swe, by = by_t, FUN = function(x) max(x)) #Don't need to divide by #yrs b/c by year
  s1 <- split(s, "time")
  
  ratio1 <- s1[2:32,,]/p1[2:32,,]
  ratio  <- s[,2:32,,]/p[,2:32,,]
  mean_fut<-st_apply(ratio, c("x", "y"), mean)
  
  df<-data.frame(year=future.period,mean=NA)
  for (i in 1:length(future.period)){
    t <-st_apply(ratio1[i],1:2,mean)
    df$mean[i] <- mean(t$mean,na.rm=TRUE)
  }
  df$GCM <- GCMs[G]; names(df) <- c("Year", var, "GCM")
  DF<-rbind(DF,df)
  delta = mean_fut - mean_grid
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))

rm(ratio,ratio1,s,s1,p,p1,ws_var,wf_var,fut_var_ws,fut_var_wf,cropped_st_wf,cropped_st_ws)
gc()
}

write.csv(DF,paste0(data.dir,"/",var,"_ANN.csv"),row.names=FALSE)
  