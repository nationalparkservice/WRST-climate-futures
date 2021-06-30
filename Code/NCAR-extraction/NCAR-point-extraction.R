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
data.dir <- "D:/NCAR_AK/met"
vic.dir <- "D:/NCAR_AK/vic_hydro"
Out.dir <- "C:/Users/achildress/DOI/NPS-WRST-Resource Stewardship Strategy - Climate/Climate futures development/Data/"
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

##############################################################################
##############################################################################
######### Create df for data ######### 
# GCM, rcp, date, pcp, tmax, tmin
daily<-as.data.frame(matrix(data=NA,nrow=0,ncol=6))
names(daily)<-c("Date","GCM","rcp","tmax","tmin","pcp")

start.time <- Sys.time()

for (G in 1:length(GCMs)){
# for (G in 1:1){
  gcm = GCMs[G]
for (R in 1:length(RCPs)){
  # for (R in 1:1){
rcp = RCPs[R]
path = paste(data.dir,gcm,rcp,sep="/")
print(paste("extracting", gcm,rcp,sep="."))
file.list<-list.files(path=path, pattern=".nc4") #list all files in folder


# # Extract model names from RCP 
# GCMs<-sub("\\_.*", "", file.list) 

######## Method working with .nc object and 'flattening' for point location
for (file in 1:length(file.list)){
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
rm(df)
nc_close(x)}
}
}
end.time<-Sys.time() 
rm(x)

daily$GCM <- paste(daily$GCM, daily$rcp, sep=".")

##############################################################################
##############################################################################
############# DAYMET EXTRACTION
daymet<-as.data.frame(matrix(data=NA,nrow=0,ncol=6))
names(daymet)<-c("Date","GCM","rcp","tmax","tmin","pcp")
   
path = paste(data.dir,"daymet",sep="/")
    
file.list<-list.files(path=path, pattern=".nc") #list all files in folder
    
######## Method working with .nc object and 'flattening' for point location
    for (file in 1:length(file.list)){
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

##############################################################################
##############################################################################
############# VIC EXTRACTION
eb.variables <- c("LATENT","SENSIBLE","SOIL_TEMP1","SOIL_TEMP2","SOIL_TEMP3")
wf.variables <- c("BASEFLOW","EVAP","GLACIER_MELT","PRCP","RUNOFF","SNOW_MELT")
wb.variables <- c("IWE","SM1","SM2","SM3","SWE","WATER_ERROR")

##############################################################################
##############################################################################
######### ENERGY BALANCE
energy.balance<-as.data.frame(matrix(data=NA,nrow=0,ncol=length(eb.variables)+3))
names(energy.balance)<-c("Date","GCM","rcp",eb.variables)

for (G in 1:length(GCMs)){
  gcm = GCMs[G]
  for (R in 1:length(RCPs)){
    rcp = RCPs[R]
    path = paste(vic.dir,"monthly/BCSD",gcm,rcp,sep="/")
    print(paste("extracting", gcm,rcp,sep="."))

    file.list<-list.files(path=path, pattern=".nc") #list all files in folder
    eb.file.list <- file.list[grep("eb", file.list)]

    for (eb in 1:length(eb.file.list)){
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
      df1$LATENT <- ncvar_get(x, "LATENT",
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
      energy.balance<-rbind(energy.balance,df1)}
  }
}

rm(df1)
rm(x)

energy.balance$GCM <- paste(energy.balance$GCM, energy.balance$rcp, sep=".")

##############################################################################
##############################################################################
########## WATER FLUX
water.flux<-as.data.frame(matrix(data=NA,nrow=0,ncol=length(wf.variables)+3))
names(water.flux)<-c("Date","GCM","rcp",wf.variables)

for (G in 1:length(GCMs)){
  gcm = GCMs[G]
  for (R in 1:length(RCPs)){
    rcp = RCPs[R]
    path = paste(vic.dir,"monthly/BCSD",gcm,rcp,sep="/")
    print(paste("extracting", gcm,rcp,sep="."))
    
    file.list<-list.files(path=path, pattern=".nc") #list all files in folder
    wf.file.list <- file.list[grep("wf", file.list)]
    
    for (wf in 1:length(wf.file.list)){
      x<-nc_open(paste0(path, "/", wf.file.list[wf])) 
      
      lon <- ncvar_get(x, "longitude") 
      lat <- ncvar_get(x, "latitude")
      
      # this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
      # it flattens the grid by converting coordinate to vectors
      
      ts<-as.POSIXct(nc.get.time.series(x))
      df1 <- as.data.frame(matrix(NA,length(ts),0))  # dummy df
      df1$Date <- ts
      
      df1$GCM <- gcm
      df1$rcp <- rcp
      
      
      dum_var <- ncvar_get(x, wf.variables[1],start=c(1,1,1),count = c(-1,-1,1)) 
      coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 
      
      coords$lat.distance<-abs(coords$lat - Lat)
      coords$lon.distance<-abs(coords$lon - Lon)
      
      coords$index<-coords$lat.distance*coords$lon.distance
      
      Index = coords$id[which.min(coords$index)]
      
      
      #convert linear ind to r and c index
      rc <- arrayInd(Index,dim(dum_var)) #get row and column index
      
      #get the variables at the required location for all time steps.
      df1$BASEFLOW<- ncvar_get(x, "BASEFLOW",  
                               start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$EVAP<- ncvar_get(x, "EVAP",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$GLACIER_MELT <- ncvar_get(x, "GLACIER_MELT",  
                                    start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$PRCP <- ncvar_get(x, "PRCP",  
                            start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$RUNOFF <- ncvar_get(x, "RUNOFF",  
                              start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$SNOW_MELT <- ncvar_get(x, "SNOW_MELT",  
                                 start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      nc_close(x)
      water.flux<-rbind(water.flux,df1)
    }}}
rm(df1)
rm(x)

water.flux$GCM <- paste(water.flux$GCM, water.flux$rcp, sep=".")


##############################################################################
##############################################################################
########## WATER BALANCE
water.balance<-as.data.frame(matrix(data=NA,nrow=0,ncol=length(wb.variables)+3))
names(water.balance)<-c("Date","GCM","rcp",wb.variables)

for (G in 1:length(GCMs)){
  gcm = GCMs[G]
  for (R in 1:length(RCPs)){
    rcp = RCPs[R]
    path = paste(vic.dir,"monthly/BCSD",gcm,rcp,sep="/")
    print(paste("extracting", gcm,rcp,sep="."))
    
    file.list<-list.files(path=path, pattern=".nc") #list all files in folder
    wb.file.list <- file.list[grep("ws", file.list)]
    
    for (wb in 1:length(wb.file.list)){
      x<-nc_open(paste0(path, "/", wb.file.list[wb])) 
      
      lon <- ncvar_get(x, "longitude") 
      lat <- ncvar_get(x, "latitude")
      
      # this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
      # it flattens the grid by converting coordinate to vectors
      
      ts<-as.POSIXct(nc.get.time.series(x))
      df1 <- as.data.frame(matrix(NA,length(ts),0))  # dummy df
      df1$Date <- ts
      
      df1$GCM <- gcm
      df1$rcp <- rcp
      
      
      dum_var <- ncvar_get(x, wb.variables[1],start=c(1,1,1),count = c(-1,-1,1)) 
      coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 
      
      coords$lat.distance<-abs(coords$lat - Lat)
      coords$lon.distance<-abs(coords$lon - Lon)
      
      coords$index<-coords$lat.distance*coords$lon.distance
      
      Index = coords$id[which.min(coords$index)]
      
      
      #convert linear ind to r and c index
      rc <- arrayInd(Index,dim(dum_var)) #get row and column index
      
      #get the variables at the required location for all time steps.
      df1$IWE <- ncvar_get(x, "IWE",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$SM1 <- ncvar_get(x, "SM1",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$SM2 <- ncvar_get(x, "SM3",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$SM3 <- ncvar_get(x, "SM3",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$SWE <- ncvar_get(x, "SWE",  
                           start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      df1$WATER_ERROR <- ncvar_get(x, "WATER_ERROR",  
                                   start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
      nc_close(x)
      water.balance<-rbind(water.balance,df1)
    }}}
rm(df1)
rm(x)

water.balance$GCM <- paste(water.balance$GCM, water.balance$rcp, sep=".")

save.image(paste(Outdir,"WRST_centroid","_init_parsed.RData",sep=""))


