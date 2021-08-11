## VIC MONTHLY
print("extracting Daymet")

model.dir <- paste0(vic.dir,"/daily/daymet")
# dir.create(model.dir,showWarnings=FALSE)
DF.grid <- data.frame()
grid_filelist = list.files(path = paste(met.dir,"daymet",sep='/'), pattern= '.nc', full.names = TRUE)

# DAYMET ----
for(i in 1:length(grid_filelist)){
# grid_annual <- list() # Create a list to put the stars objects into
yr = as.POSIXct(sub('.*\\met_', '', sub("\\..*", "", grid_filelist[i])),format="%Y")
invisible(capture.output(
  suppressWarnings(
    grid_star = read_stars(grid_filelist[i], sub=c("tmax","tmin","pcp") ,curvilinear = c("longitude", "latitude")))))
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
  mutate(over20 = tmax > 20)-> grid_threshold
  # mutate(month = as.numeric(format(st_get_dimension_values(grid_crop, 'time'),"%m"))) 
  # mutate(hot.pctl = quantile(tmax_f,.99,na.rm=TRUE)) 

freeze.thaw.sum <- st_apply((grid_threshold %>% dplyr::select(freeze.thaw)), c("x", "y"), sum, rename=FALSE) 
GDD.sum <- st_apply((grid_threshold %>% dplyr::select(GDD)), c("x", "y"), sum,rename=FALSE)
under32.sum <- st_apply((grid_threshold %>% dplyr::select(under32)), c("x", "y"), sum,rename=FALSE)
over20.sum <- st_apply((grid_threshold %>% dplyr::select(over20)), c("x", "y"), sum,rename=FALSE)

# Don't know how to add/retain time dimension for year

by_t = "1 month"
under32.month = aggregate((grid_threshold %>% dplyr::select(under32)), by = by_t, FUN = sum)
# under32.split <- split(under32.month, "time")

WSF.below32 <- st_apply(under32.month[,c(1:5,9:12)],c("x", "y"),sum,rename=FALSE)
names(WSF.below32) <- "WSF.below32"
W.under32 <- st_apply(under32.month[,c(1:2,12)],c("x", "y"),sum,rename=FALSE)
names(W.under32) <- "W.under32"

# grid_annual[[i]]<- 
annual_thresholds <- c(freeze.thaw.sum, GDD.sum,under32.sum,over20.sum,WSF.below32,W.under32)

## timeseries df
df<-data.frame(GCM = "Daymet",year = yr)
s <- st_apply(annual_thresholds,1:2,mean)
df$freeze.thaw = mean(s$freeze.thaw,na.rm=TRUE)
df$GDD = mean(s$GDD,na.rm=TRUE)
df$under32 = mean(s$under32,na.rm=TRUE)
df$over20 = mean(s$over20)
df$WSF.below32 = mean(s$WSF.below32,na.rm=TRUE)
df$W.under32 = mean(s$W.under32,na.rm=TRUE)

DF.grid[[i]] <- df
rm(annual_thresholds,freeze.thaw.sum, GDD.sum,under32.sum,Over20.sum,WSF.below32,W.under32,
   grid_threshold,grid_crop)
gc()
}
write.csv(DF.grid,paste0(model.dir,"/Annual_thresholds_Daymet.csv"),row.names = TRUE)

# FUTURE ----

DF.fut <- data.frame()
for (G in 1:length(GCMs)){
  # setting variables ----
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  path = paste(vic.dir,"daily/BCSD", gcm, rcp, sep = '/')
  
  file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
  fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
  wf_fut_filelist <- fut_filelist[grep("wf", fut_filelist)]
  ws_fut_filelist <- fut_filelist[grep("ws", fut_filelist)]
  
  Pr.99th <- list()
  for(i in 1:length(future_filelist)){
  yr = as.POSIXct(sub('.*\\wf_', '', sub("\\..*", "", wf_fut_filelist[i])),format="%Y")
  print(yr)
  # invisible(capture.output(
  #   suppressWarnings(
  fut_star_wf = read_stars(wf_fut_filelist[i], sub=c("PRCP","RUNOFF") ,curvilinear = c("longitude", "latitude")) #)))
  fut_star_wf = st_transform(fut_star_wf, st_crs(shp))
  fut_crop_wf = fut_star_wf[shp]
  fut_crop_wf = drop_units(fut_crop_wf)
  
  fut_star_ws = read_stars(ws_fut_filelist[i], sub=c("SWE") ,curvilinear = c("longitude", "latitude")) #)))
  fut_star_ws = st_transform(fut_star_ws, st_crs(shp))
  fut_crop_ws = fut_star_ws[shp]
  fut_crop_ws = drop_units(fut_crop_ws)
  
  rm(fut_star_wf, fut_star_ws)
  
  # add Imperial units
  fut_crop_wf %>% mutate(RUNOFF_in = RUNOFF / 25.4) %>%
    mutate(PCP_in = PRCP / 25.4) %>%
    select(c(RUNOFF_in, PCP_in)) -> wf_imperial
  
  fut_crop_ws %>% mutate(SWE_in = SWE / 25.4) %>% select(SWE) -> ws_imperial
  
  # Daily mean values
  runoff.mean <- st_apply((wf_imperial %>% dplyr::select(RUNOFF_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
  pcp.mean <- st_apply((wf_imperial %>% dplyr::select(PCP_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
  SWE.mean <- st_apply((ws_imperial %>% dplyr::select(SWE_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
  
  full_join(data.frame(runoff.mean), data.frame(pcp.mean), by = "time") -> df 
  full_join(df, data.frame(SWE.mean), by = "time") -> df
  DF.fut <- rbind(DF.fut,df)
  rm(runoff.mean,SWE.mean,fut_crop_ws,imperial_ws,fut_crop_wf)
  
  
  head(df)
  
  # Do this for Daymet
  
  yrs <- c(as.POSIXct("2025",format="%Y"),as.POSIXct("2026",format="%Y"))
  pr.99 <- st_apply((wf_imperial %>% dplyr::select(PCP_in)), c("x","y"), FUN=function(x) quantile((x),0.99,na.rm=TRUE))
  pr.91 <- st_apply((wf_imperial %>% dplyr::select(PCP_in)), c("x","y"), FUN=function(x) quantile((x),0.99,na.rm=TRUE)+1)
  
  
  names(pr.99) <- yr
  pr.99 = st_redimension(pr.99)
pr.list <- list()
  pr.list[[1]]<-pr.99
  pr.list[[2]] <- pr.91
names(pr.list)= yrs
  pr.list[[2]] = st_redimension(pr.list[[2]])
  
  r = c(do.call("c", pr.list))
  r = st_redimension(r, new_dims = st_dimensions(r), along = list(new_dim = names(r)))
  r
    
  
  r= (c(pr.99, pr.91), along = "new_dimensions")

  
  names(r) = yrs
  r = st_redimension(r)
  r
  
  r = list()
  for(i in paths) {
    files = list.files(path = i, pattern = "\\.tif$", full.names = TRUE)
    r[[i]] = read_stars(files)
    names(r[[i]]) = basename(files)
    r[[i]] = st_redimension(r[[i]])
  }
  
  st <- st_apply(c(pr.99, pr.91),MARGIN=c("x","y"),mean, na.rm=TRUE)
st %>%  
  summarise(mean = mean(PCP_in,PCP_in.1)) 
st1 <- st_apply(st[1:2],c("x","y"),sum)


# 
pr.df<- data.frame()
pr1 <- as.data.frame(pr.99)
pr1$year = yr
pr.df <- rbind(pr.df,pr1)
head(pr.df)
pr.agg <- aggregate(PCP_in~x+y,pr.df,sum)
Sp_obj <- st_as_sf(pr.agg, coords = c('x', 'y'), crs = 3338)

stars_obj<-st_as_stars(Sp_obj,name = attr(Sp_obj, "PCP_in"))
stars_obj<-st_as_stars(Sp_obj,curvilinear=geometry)
  
  
  
st_redimension(pr.99,   new_dims = c(st_dimensions(pr.99),year),along = list(year=yr)) -> r
  

dim(r)[3]
  r <-  st_set_dimensions(r, 3, from=1,to=1,offset=NA,delta=NA,refsys=POSIXct, point = NA, values=yr)
  
  S <- list()
  for(F in 1:dim(wf_imperial)[3]){
    s = wf_imperial[,,,F]
    s = st_apply(s,1:2,mean)
    s <- c(s,pr.99)
    s %>% mutate(pr.exceed = PCP_in > pr.99) -> s
    S[[F]] = s
  }
  s1<-st_apply((S %>% dplyr::select(pr.exceed)), c("x","y"), mean)
  s1 <- Reduce((x,y),S)
  
  

  
  
  
      
      ## timeseries df
      df<-data.frame(GCM = GCMs[G],year = yr)
      s <- st_apply(annual_thresholds,1:2,mean)
      df$freeze.thaw = mean(s$freeze.thaw,na.rm=TRUE)
      df$GDD = mean(s$GDD,na.rm=TRUE)
      df$under32 = mean(s$under32,na.rm=TRUE)
      df$over20 = mean(s$over20,na.rm=TRUE)
      df$WSF.below32 = mean(s$WSF.below32,na.rm=TRUE)
      df$W.under32 = mean(s$W.under32,na.rm=TRUE)

      DF.fut <- rbind(DF.fut,df)
      
      rm(annual_thresholds,freeze.thaw.sum, GDD.sum,under32.sum,Over20.sum,WSF.below32,W.under32,
         fut_threshold,fut_crop)
      gc()
      # )
}
write.csv(DF.fut,paste0(data.dir,"/Annual_thresholds_",gcm,"_",rcp,".csv"),row.names = TRUE)
}
