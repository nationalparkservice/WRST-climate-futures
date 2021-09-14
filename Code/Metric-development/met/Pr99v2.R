## MET MONTHLY
var = "Pr99v2"
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
    (grid_star = read_stars(grid_filelist[i], sub=c("pcp") ,curvilinear = c("longitude", "latitude"))))))
grid_star = st_transform(grid_star, st_crs(shp))
grid_crop = grid_star[shp]
names(grid_crop) = "pcp"
grid_crop = drop_units(grid_crop)
rm(grid_star)

# add Imperial units

grid_crop %>% mutate(pcp_in = pcp / 25.4) -> grid_crop

# add threshold var
grid_crop %>% mutate(pctl = quantile(pcp_in,.99, na.rm=TRUE)) -> grid_crop

pr.99 <- st_apply((grid_crop %>% dplyr::select(pcp_in)), c("x","y"), FUN=function(x) quantile((x),0.99,na.rm=TRUE))
assign(paste0("pr",i),pr.99)
rm(pr.99,grid_crop)
}

pr.list = list()
yrs = as.POSIXct(daymet.period,format="%Y")
for (j in 1:length(daymet.period)){
 pr.list[[j]] = eval(parse(text=paste0("pr",j)))
}

## NOTE: for redimension MUST use stars 0.5-4 or greater, as of 9/11/21, install using "devtools::install_github("r-spatial/stars")"
r = do.call("c", pr.list)  
st_redimension(r) %>%
  st_set_dimensions(3, values=yrs, names = "time") %>%
  setNames("pcp_in") -> r

# saveRDS(r, file = paste(data.dir,paste(var,"daymet",sep="_"),sep="/"))
mean_hist <- st_apply(r, c("x", "y"), mean) # find mean

  pcp.time <- st_apply((r %>% dplyr::select(pcp_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
 
  df <- data.frame(pcp.time)
  df$GCM = "Daymet"; names(df) <- c("Year", var, "GCM")
  DF = rbind(DF, df)  
  
rm(pr.list)
gc()


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
      (fut_star = read_ncdf(fut_filelist[i], var=c("pcp"), curvilinear = c("longitude", "latitude"))))))
      fut_star = st_transform(fut_star, st_crs(shp))
      fut_crop = fut_star[shp]
      fut_crop = drop_units(fut_crop)
      rm(fut_star)
      
      # add Imperial units
      fut_crop %>% mutate(pcp_in = pcp / 25.4) -> fut_crop
    
      # add threshold var
      fut_crop %>% mutate(pctl = quantile(pcp_in,.99, na.rm=TRUE)) -> fut_crop
      
      pr.99 <- st_apply((fut_crop %>% dplyr::select(pcp_in)), c("x","y"), FUN=function(x) quantile((x),0.99,na.rm=TRUE))
      assign(paste0("pr",i),pr.99)
      rm(pr.99,fut_crop)
    }
    
    pr.list = list()
    yrs = as.POSIXct(future.period,format="%Y")
    for (j in 1:length(future.period)){
      pr.list[[j]] = eval(parse(text=paste0("pr",j)))
    }
    
    ## NOTE: for redimension MUST use stars 0.5-4 or greater, as of 9/11/21, install using "devtools::install_github("r-spatial/stars")"
    r = do.call("c", pr.list)  
    st_redimension(r) %>%
      st_set_dimensions(3, values=yrs, names = "time") %>%
      setNames("pcp_in") -> r
    
    mean_fut <- st_apply(r, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    pcp.time <- st_apply((r %>% dplyr::select(pcp_in)), c("time"),mean,na.rm=TRUE, rename=FALSE)
    
    df <- data.frame(pcp.time)
    df$GCM <- GCMs[G]; names(df) <- c("Year", var, "GCM")

      DF <- rbind(DF,df)
      
      rm(pr.list)
      gc()
      # )
}


write.csv(DF,paste0(data.dir,"/",var,"_ANN.csv"),row.names=FALSE)

