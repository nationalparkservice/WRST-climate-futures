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
data.dir<-"C:/Users/achildress/Documents/NCAR-test/met/" #location data file

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
data<-as.data.frame(matrix(data=NA,nrow=0,ncol=6))
names(data)<-c("Date","GCM","rcp","tmax","tmin","pcp")


for (R in RCPs){
rcp = RCPs[R]

file.list<-list.files(path=paste0(data.dir, rcp), pattern=".nc4") #list all files in folder

# Extract model names from RCP 
GCMs<-sub("\\_.*", "", file.list) 

######## Method working with .nc object and 'flattening' for point location
for (file in length(file.list)){
gcm = sub("\\_.*", "", file.list[file])
# year<-sub('.*[_]([^.]+)[.].*', "\\1", file.list[file]) #syntax explanation https://stackoverflow.com/questions/23518325/how-to-extract-substring-between-patterns-and-in-r


x<-nc_open(paste0(paste0(data.dir, rcp, "/", file.list[file]))) 

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
coords$lon.distance<-abs(coords$lon -cLon)

coords$index<-coords$lat.distance*coords$lon.distance

Index = coords$id[which.min(coords$index)]

Lat_index = as.numeric(which.min(abs(coords$lat - Lat))) #returns cell index that is closest to Lat var
Lon_index = as.numeric(which.min(abs(coords$lon -cLon)))#returns cell index that is closest to Lon var

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
data<-rbind(data,df)

nc_close(x)
}
}
rm(df)

data$GCM <- paste(data$GCM, data$rcp, sep=".")
