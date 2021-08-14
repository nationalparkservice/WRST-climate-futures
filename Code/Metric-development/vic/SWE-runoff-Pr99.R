# Daily SWE ; Daily Runoff ----
var1 = "SWE"
var2 = "runoff"
DF1 <- data.frame()
DF2 <- data.frame()

print("extracting Daymet")

# model.dir <- paste0(vic.dir,"/daily/daymet")
# dir.create(model.dir,showWarnings=FALSE)
grid_filelist = list.files(path = paste0(vic.dir,"/daily/daymet"), pattern= '.nc', full.names = TRUE)
wf_grid_filelist <- grid_filelist[grep("wf", grid_filelist)]
ws_grid_filelist <- grid_filelist[grep("ws", grid_filelist)]


# DAYMET ----
for(i in 1:length(wf_grid_filelist)){
yr = as.POSIXct(sub('.*\\wf_', '', sub("\\..*", "", wf_grid_filelist[i])),format="%Y")
print(yr)
invisible(capture.output(suppressWarnings(
  (grid_star_wf = read_stars(wf_grid_filelist[i], sub=c("RUNOFF") ,curvilinear = c("longitude", "latitude"))))))
grid_star_wf = st_transform(grid_star_wf, st_crs(shp))
grid_crop_wf = grid_star_wf[shp]
grid_crop_wf = drop_units(grid_crop_wf)

invisible(capture.output(
  suppressWarnings(
    (grid_star_ws = read_stars(ws_grid_filelist[i], sub=c("SWE") ,curvilinear = c("longitude", "latitude"))) )))
grid_star_ws = st_transform(grid_star_ws, st_crs(shp))
grid_crop_ws = grid_star_ws[shp]
grid_crop_ws = drop_units(grid_crop_ws)

rm(grid_star_wf, grid_star_ws)

# add Imperial units
grid_crop_wf %>% mutate(RUNOFF_in = RUNOFF / 25.4)  -> wf_imperial

grid_crop_ws %>% mutate(SWE_in = SWE / 25.4) -> ws_imperial

# Daily mean values
runoff.mean <- st_apply((wf_imperial %>% dplyr::select(RUNOFF_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
# pcp.mean <- st_apply((wf_imperial %>% dplyr::select(PCP_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
SWE.mean <- st_apply((ws_imperial %>% dplyr::select(SWE_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)

df1 <- data.frame(SWE.mean)
df1$GCM = "Daymet"
df2 <- data.frame(runoff.mean)
df2$GCM = "Daymet"

DF1 <- rbind(DF1,df1)
DF2 <- rbind(DF2, df2)
}
rm(runoff.mean,SWE.mean,grid_crop_ws,grid_crop_wf)
gc()

# FUTURE ----

for (G in 1:length(GCMs)){
  # setting variables ----
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  path = paste(vic.dir,"daily/BCSD", gcm, rcp, sep = '/')
  
  file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
  fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
  wf_fut_filelist <- fut_filelist[grep("wf", fut_filelist)]
  ws_fut_filelist <- fut_filelist[grep("ws", fut_filelist)]
  
  # Pr.99th <- list()
  for(i in 1:length(wf_fut_filelist)){
  yr = as.POSIXct(sub('.*\\wf_', '', sub("\\..*", "", wf_fut_filelist[i])),format="%Y")
  print(yr)
  invisible(capture.output(suppressWarnings(
  (fut_star_wf = read_stars(wf_fut_filelist[i], sub=c("RUNOFF") ,curvilinear = c("longitude", "latitude"))))))
  fut_star_wf = st_transform(fut_star_wf, st_crs(shp))
  fut_crop_wf = fut_star_wf[shp]
  fut_crop_wf = drop_units(fut_crop_wf)
  
  invisible(capture.output(
    suppressWarnings(
  (fut_star_ws = read_stars(ws_fut_filelist[i], sub=c("SWE") ,curvilinear = c("longitude", "latitude"))) )))
  fut_star_ws = st_transform(fut_star_ws, st_crs(shp))
  fut_crop_ws = fut_star_ws[shp]
  fut_crop_ws = drop_units(fut_crop_ws)
  
  rm(fut_star_wf, fut_star_ws)
  
  # add Imperial units
  fut_crop_wf %>% mutate(RUNOFF_in = RUNOFF / 25.4)  -> wf_imperial
  
  fut_crop_ws %>% mutate(SWE_in = SWE / 25.4)  -> ws_imperial
  
  # Daily mean values
  runoff.mean <- st_apply((wf_imperial %>% dplyr::select(RUNOFF_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
  # pcp.mean <- st_apply((wf_imperial %>% dplyr::select(PCP_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
  SWE.mean <- st_apply((ws_imperial %>% dplyr::select(SWE_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
  
  df1 <- data.frame(SWE.mean)
  df1$GCM = GCMs[G]
  df2 <- data.frame(runoff.mean)
  df2$GCM = GCMs[G]
  
  DF1 <- rbind(DF1,df1)
  DF2 <- rbind(DF2,df2)
  rm(runoff.mean,SWE.mean,fut_crop_ws,ws_imperial,wf_imperial,fut_crop_wf)
  gc()
  }
}

write.csv(DF1,paste0(data.dir,"/", var1, "_DAY.csv"),row.names = TRUE) 
write.csv(DF2,paste0(data.dir,"/", var2, "_DAY.csv"),row.names = TRUE)

# #Code to get Pr99 to work -- does not work, submitted issue to stars GitHub repo
#   # Do this for Daymet
# 
#   yrs <- c(as.POSIXct("2025",format="%Y"),as.POSIXct("2026",format="%Y"))
#   pr.99 <- st_apply((wf_imperial %>% dplyr::select(PCP_in)), c("x","y"), FUN=function(x) quantile((x),0.99,na.rm=TRUE))
#   pr.91 <- st_apply((wf_imperial %>% dplyr::select(PCP_in)), c("x","y"), FUN=function(x) quantile((x),0.99,na.rm=TRUE)+1)
# 
# 
#   names(pr.99) <- yr
#   pr.99 = st_redimension(pr.99)
# pr.list <- list()
#   pr.list[[1]]<-pr.99
#   pr.list[[2]] <- pr.91
# names(pr.list)= yrs
#   pr.list[[2]] = st_redimension(pr.list[[2]])
# 
#   r = do.call("c", pr.list)
#   r = st_redimension(r, new_dims = st_dimensions(r), along = list(new_dim = names(r)))
#   
#   yrs <- c(as.POSIXct("2025",format="%Y"),as.POSIXct("2026",format="%Y"))
#   st_redimension(r)  %>%
#     st_set_dimensions(3, values=yrs, names = "year") %>%
#     setNames("PCP_in")
#   
#   saveRDS(r,"C:/Users/achildress/Downloads/r.rds" )
#   
#   yrs=as.Date(c("2025.08.08","2026.08.08"),"%Y.%M.%d")
#   st_redimension(r) %>%
#     st_set_dimensions(3, yrs, names="year")
#   
#   dates = as.Date(substr(names(data), 16, 24), "%Y.%M.%d")
#   st_redimension(data) %>% 
#     st_set_dimensions(3, dates, names = "Date") %>%
#     setNames("ESACCI6.1.mnth")
#     
#   
#   r= (c(pr.99, pr.91), along = "new_dimensions")
# 
#   
#   names(r) = yrs
#   r = st_redimension(r)
#   r
#   
#   r = list()
#   for(i in paths) {
#     files = list.files(path = i, pattern = "\\.tif$", full.names = TRUE)
#     r[[i]] = read_stars(files)
#     names(r[[i]]) = basename(files)
#     r[[i]] = st_redimension(r[[i]])
#   }
#   
#   st <- st_apply(c(pr.99, pr.91),MARGIN=c("x","y"),mean, na.rm=TRUE)
# st %>%  
#   summarise(mean = mean(PCP_in,PCP_in.1)) 
# st1 <- st_apply(st[1:2],c("x","y"),sum)
# 
# 
# # 
# pr.df<- data.frame()
# pr1 <- as.data.frame(pr.99)
# pr1$year = yr
# pr.df <- rbind(pr.df,pr1)
# head(pr.df)
# pr.agg <- aggregate(PCP_in~x+y,pr.df,sum)
# Sp_obj <- st_as_sf(pr.agg, coords = c('x', 'y'), crs = 3338)
# 
# stars_obj<-st_as_stars(Sp_obj,name = attr(Sp_obj, "PCP_in"))
# stars_obj<-st_as_stars(Sp_obj,curvilinear=geometry)
#   
#   
#   
# st_redimension(pr.99,   new_dims = c(st_dimensions(pr.99),year),along = list(year=yr)) -> r
#   
# 
# dim(r)[3]
#   r <-  st_set_dimensions(r, 3, from=1,to=1,offset=NA,delta=NA,refsys=POSIXct, point = NA, values=yr)
#   
#   S <- list()
#   for(F in 1:dim(wf_imperial)[3]){
#     s = wf_imperial[,,,F]
#     s = st_apply(s,1:2,mean)
#     s <- c(s,pr.99)
#     s %>% mutate(pr.exceed = PCP_in > pr.99) -> s
#     S[[F]] = s
#   }
#   s1<-st_apply((S %>% dplyr::select(pr.exceed)), c("x","y"), mean)
#   s1 <- Reduce((x,y),S)
#   
#   

