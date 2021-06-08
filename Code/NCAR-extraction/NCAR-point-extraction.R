library(ncdf4)
library(reshape2)
library(raster)
library(stars)
library(dplyr)
library('ncdf4.helpers')
library('maptools')
library(ggplot2) #for plotting
library(units) # for dropping units


rm(list=ls())

######### MET parsing - file location ######### 
# data.dir<-"C:/Users/achildress/Documents/NCAR-test/met/" #location data file
data.dir <- "D:/AK/met"
vic.dir <- "D:/AK/vic_hydro"
GCMs <- as.list(list.dirs(path = data.dir, full.names = FALSE, recursive = FALSE))

GCMs[which(GCMs %in% c("daymet","monthly"))] <- NULL

RCPs <- c("rcp45", "rcp85")

######### Points for extraction ######### 
Lat = 62.23494 #park centroid lat -- if doing multiple sites, read in points file, code below
Lon = -142.572 #park centroid lon
cLon = Lon + 360 #Adjusts negative lon. values 

# # Use this code when reading in point locations to extract from
# nps_boundary <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids/nps_boundary_centroids.shp')
# park <- filter(nps_boundary, UNIT_CODE == "WRST") # subset to WRST only
# Sp_park <- as_Spatial(park[1,])


######### Create df for data ######### 
# GCM, rcp, date, pcp, tmax, tmin
daily<-as.data.frame(matrix(data=NA,nrow=0,ncol=6))
names(daily)<-c("Date","GCM","rcp","tmax","tmin","pcp")

for (G in 1:length(GCMs)){
  gcm = GCMs[G]
for (R in 1:length(RCPs)){
rcp = RCPs[R]
path = paste(data.dir,gcm,rcp,sep="/")

file.list<-list.files(path=path, pattern=".nc4") #list all files in folder


# # Extract model names from RCP 
# GCMs<-sub("\\_.*", "", file.list) 

######## Method working with .nc object and 'flattening' for point location
for (file in length(file.list)){
# gcm = sub("\\_.*", "", file.list[file.list])
# year<-sub('.*[_]([^.]+)[.].*', "\\1", file.list[file]) #syntax explanation https://stackoverflow.com/questions/23518325/how-to-extract-substring-between-patterns-and-in-r


x<-nc_open(paste0(path, "/", file.list[file])) 

lon <- ncvar_get(x, "longitude") 
lat <- ncvar_get(x, "latitude")
 
# this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
# it flattens the grid by converting coordinate to vectors

ts<-as.POSIXct(nc.get.time.series(x))
df <- as.data.frame(matrix(NA,length(ts),0))  # dummy df
df$Date <- ts

df$GCM <- gcm
df$rcp <- rcp


dum_var <- ncvar_get(x, "tmax",start=c(1,1,1),count = c(-1,-1,1)) 
coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 

coords$lat.distance<-abs(coords$lat - Lat)
coords$lon.distance<-abs(coords$lon - cLon)

coords$index<-coords$lat.distance*coords$lon.distance

Index = coords$id[which.min(coords$index)]

  #convert linear ind to r and c index
  rc <- arrayInd(Index,dim(dum_var)) #get row and column index
  
  #get the variables at the required location for all time steps.
  df$tmax<- ncvar_get(x, "tmax",  
                       start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  df$tmin<- ncvar_get(x, "tmin",  
                   start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  df$pcp <- ncvar_get(x, "pcp",  
                   start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  
  # add other variables in output here
daily<-rbind(daily,df)

nc_close(x)
}}}
rm(df)
rm(x)

daily$GCM <- paste(data$GCM, data$rcp, sep=".")


############# DAYMET EXTRACTION
daymet<-as.data.frame(matrix(data=NA,nrow=0,ncol=6))
names(daymet)<-c("Date","GCM","rcp","tmax","tmin","pcp")
   
path = paste(data.dir,"daymet",sep="/")
    
file.list<-list.files(path=path, pattern=".nc") #list all files in folder
    
######## Method working with .nc object and 'flattening' for point location
    for (file in length(file.list)){
      # gcm = sub("\\_.*", "", file.list[file.list])
      # year<-sub('.*[_]([^.]+)[.].*', "\\1", file.list[file]) #syntax explanation https://stackoverflow.com/questions/23518325/how-to-extract-substring-between-patterns-and-in-r
      
      
      x<-nc_open(paste0(path, "/", file.list[file])) 
      
      lon <- ncvar_get(x, "longitude") 
      lat <- ncvar_get(x, "latitude")
      
      # this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
      # it flattens the grid by converting coordinate to vectors
      
      ts<-as.POSIXct(nc.get.time.series(x))
      df <- as.data.frame(matrix(NA,length(ts),0))  # dummy df
      df$Date <- ts
      
      df$GCM <- "gridmet"
      df$rcp <- NA
      
      
      dum_var <- ncvar_get(x, "tmax",start=c(1,1,1),count = c(-1,-1,1)) 
      coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 
      
      coords$lat.distance<-abs(coords$lat - Lat)
      coords$lon.distance<-abs(coords$lon - Lon)
      
      coords$index<-coords$lat.distance*coords$lon.distance
      
      Index = coords$id[which.min(coords$index)]

      #convert linear ind to r and c index
      rc <- arrayInd(Index,dim(dum_var)) #get row and column index
      
      #get the variables at the required location for all time steps.
      df$tmax<- ncvar_get(x, "tmax",  
                          start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df$tmin<- ncvar_get(x, "tmin",  
                          start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df$pcp <- ncvar_get(x, "pcp",  
                          start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      
      # add other variables in output here
      daymet<-rbind(daymet,df)
      
      nc_close(x)
    }
rm(df)
rm(x)


############# VIC EXTRACTION
eb.variables <- c("ENERGY_ERROR","GRND_FLUX","LATENT","NET_LONG","NET_SHORT","SENSIBLE","SOIL_TEMP1","SOIL_TEMP2","SOIL_TEMP3")
wf.variables <- c("BASEFLOW","EVAP","GLACIER_MELT","PRCP","RUNOFF","SNOW_MELT")
ws.variables <- c("IWE","SM1","SM2","SM3","SWE","WATER_ERROR")

# need to separate out those that contain eb, wf, and wb
#identify variables (names(x$var))

# GCM, rcp, date, pcp, tmax, tmin
vic<-as.data.frame(matrix(data=NA,nrow=0,ncol=length(eb.variables)+length(wf.variables)+length(ws.variables)+3))
names(vic)<-c("Date","GCM","rcp",eb.variables,wf.variables,ws.variables)

for (G in 1:length(GCMs)){
  gcm = GCMs[G]
  for (R in 1:length(RCPs)){
    rcp = RCPs[R]
    path = paste(vic.dir,"daily/BCSD",gcm,rcp,sep="/")
    
    file.list<-list.files(path=path, pattern=".nc4") #list all files in folder
   
    eb.file.list <- file.list[grep("eb", file.list)]
    wf.file.list <- file.list[grep("wf", file.list)]
    ws.file.list <- file.list[grep("ws", file.list)]
  
    for (eb in length(eb.file.list)){
      x<-nc_open(paste0(path, "/", eb.file.list[eb])) 
    
    lon <- ncvar_get(x, "longitude") 
    lat <- ncvar_get(x, "latitude")
    
    # this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
    # it flattens the grid by converting coordinate to vectors
    
    ts<-as.POSIXct(nc.get.time.series(x))
    df1 <- as.data.frame(matrix(NA,length(ts),0))  # dummy df
    df1$Date <- ts
    
    df1$GCM <- gcm
    df1$rcp <- rcp
    
    
    dum_var <- ncvar_get(x, eb.variables[1],start=c(1,1,1),count = c(-1,-1,1)) 
    coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 
    
    coords$lat.distance<-abs(coords$lat - Lat)
    coords$lon.distance<-abs(coords$lon - Lon)
    
    coords$index<-coords$lat.distance*coords$lon.distance
    
    Index = coords$id[which.min(coords$index)]

    
    #convert linear ind to r and c index
    rc <- arrayInd(Index,dim(dum_var)) #get row and column index
    
    #get the variables at the required location for all time steps.
    df1$ENERGY_ERROR<- ncvar_get(x, "ENERGY_ERROR",  
                        start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
    df1$GRND_FLUX<- ncvar_get(x, "GRND_FLUX",  
                        start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
    df1$LATENT <- ncvar_get(x, "LATENT",  
                        start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
    df1$NET_LONG <- ncvar_get(x, "NET_LONG",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
    df1$NET_SHORT <- ncvar_get(x, "NET_SHORT",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
    df1$SENSIBLE <- ncvar_get(x, "SENSIBLE",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
    df1$SOIL_TEMP1 <- ncvar_get(x, "SOIL_TEMP1",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
    df1$SOIL_TEMP2 <- ncvar_get(x, "SOIL_TEMP2",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
    df1$SOIL_TEMP3 <- ncvar_get(x, "SOIL_TEMP3",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
        nc_close(x)
  }
  for (wf in length(wf.file.list)){
    x<-nc_open(paste0(path, "/", wf.file.list[wf])) 
  
  lon <- ncvar_get(x, "longitude") 
  lat <- ncvar_get(x, "latitude")
  
  # this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
  # it flattens the grid by converting coordinate to vectors
  
  ts<-as.POSIXct(nc.get.time.series(x))
  df2 <- as.data.frame(matrix(NA,length(ts),0))  # dummy df
  df2$Date <- ts
  
  df2$GCM <- gcm
  df2$rcp <- rcp
  
  
  dum_var <- ncvar_get(x, wf.variables[1],start=c(1,1,1),count = c(-1,-1,1)) 
  coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 
  
  coords$lat.distance<-abs(coords$lat - Lat)
  coords$lon.distance<-abs(coords$lon - Lon)
  
  coords$index<-coords$lat.distance*coords$lon.distance
  
  Index = coords$id[which.min(coords$index)]

  #convert linear ind to r and c index
  rc <- arrayInd(Index,dim(dum_var)) #get row and column index
  
  #get the variables at the required location for all time steps.
  df2$BASEFLOW<- ncvar_get(x, "BASEFLOW",  
                              start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  df2$EVAP<- ncvar_get(x, "EVAP",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  df2$GLACIER_MELT <- ncvar_get(x, "GLACIER_MELT",  
                         start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  df2$PRCP <- ncvar_get(x, "PRCP",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  df2$RUNOFF <- ncvar_get(x, "RUNOFF",  
                            start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  df2$SNOW_MELT <- ncvar_get(x, "SNOW_MELT",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  
  # add other variables in output here

  nc_close(x)
  df <- merge(df1,df2,by=c("Date","GCM","rcp"))
}
for (ws in length(ws.file.list)){
  x<-nc_open(paste0(path, "/", ws.file.list[ws])) 

lon <- ncvar_get(x, "longitude") 
lat <- ncvar_get(x, "latitude")

# this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
# it flattens the grid by converting coordinate to vectors

ts<-as.POSIXct(nc.get.time.series(x))
df3 <- as.data.frame(matrix(NA,length(ts),0))  # dummy df
df3$Date <- ts

df3$GCM <- gcm
df3$rcp <- rcp


dum_var <- ncvar_get(x, ws.variables[1],start=c(1,1,1),count = c(-1,-1,1)) 
coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 

coords$lat.distance<-abs(coords$lat - Lat)
coords$lon.distance<-abs(coords$lon -Lon)

coords$index<-coords$lat.distance*coords$lon.distance

Index = coords$id[which.min(coords$index)]

#convert linear ind to r and c index
rc <- arrayInd(Index,dim(dum_var)) #get row and column index

#get the variables at the required location for all time steps.
df3$IWE <- ncvar_get(x, "IWE",  
                         start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
df3$SM1 <- ncvar_get(x, "SM1",  
                     start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
df3$SM2 <- ncvar_get(x, "SM3",  
                              start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
df3$SM3 <- ncvar_get(x, "SM3",  
                      start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
df3$SWE <- ncvar_get(x, "SWE",  
                        start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
df3$WATER_ERROR <- ncvar_get(x, "WATER_ERROR",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps

nc_close(x)
df <- merge(df,df3,by=c("Date","GCM","rcp"))
}   

vic<-rbind(vic,df)  
     
}}
rm(df1,df2,df3)
rm(x)

daily$GCM <- paste(data$GCM, data$rcp, sep=".")



