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

#### MAKE SEPARATE 4.5 AND 8.5 FILE LISTS
file.list<-list.files(path=data.dir, pattern=".nc4") #list all files by var


######### Points for extraction ######### 
Lat = 62.23494 #park centroid lat -- if doing multiple sites, read in points file, code below
Lon = -142.572 #park centroid lon
cLon = Lon + 360 #Adjusts negative lon. values 

# # Use this code when reading in point locations to extract from
# nps_boundary <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids/nps_boundary_centroids.shp')
# park <- filter(nps_boundary, UNIT_CODE == "WRST") # subset to WRST only
# Sp_park <- as_Spatial(park[1,])


######### Parse model names ######### 
# Extract model names from RCP 4.5, need to run loop through 4.5 and 8.5
GCMs<-sub("\\_.*", "", file.list)


######### Create df for data ######### 
# GCM, rcp, date, pcp, tmax, tmin
data<-as.data.frame(matrix(data=NA,nrow=length(file.list),ncol=6))
names(data)<-c("GCM","rcp","date","pcp","tmax","tmin")

######## Method working with .nc object and 'flattening' for point location
for (file in length(file.list))
file=1
gcm = sub("\\_.*", "", file.list[file])
year<-sub('.*[_]([^.]+)[.].*', "\\1", file.list[file]) #syntax explanation https://stackoverflow.com/questions/23518325/how-to-extract-substring-between-patterns-and-in-r


x<-nc_open(paste0(data.dir, file.list[file])) 

lon <- ncvar_get(x, "longitude") 
lat <- ncvar_get(x, "latitude")
 
# this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
# it flattens the grid by converting coordinate to vectors

ts<-as.POSIXct(nc.get.time.series(x))
dum_var <- ncvar_get(x, req_var,start=c(1,1,1),count = c(-1,-1,1)) 
coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 

coords$lat.distance<-abs(coords$lat - Lat)
coords$lon.distance<-abs(coords$lon -cLon)

coords$index<-coords$lat.distance*coords$lon.distance

Index = coords$id[which.min(coords$index)]

Lat_index = as.numeric(which.min(abs(coords$lat - Lat))) #returns cell index that is closest to Lat var
Lon_index = as.numeric(which.min(abs(coords$lon -cLon)))#returns cell index that is closest to Lon var

file.data <- rep(0,length(ts)) #create zeros vector to output the average value

id <-c(Index) # c(Lat_index,Lon_index) # index of cells within the mask shapefile -- single value if single cell, multi values if multi cells

req_var<-names(x$var)[4] #tmax UPDATE THIS TO 

for (j in 1:length(id)) {
  
  pt_id <- id[j]
  #convert linear ind to r and c index
  rc <- arrayInd(pt_id,dim(dum_var)) #get row and column index
  
  #get the variable at the required location for all time steps.
  
  pt_data <- ncvar_get(x, req_var,  
                       start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  file.data <- file.data+pt_data #sum data to be averaged after the loop ## Doesn't work if NA values
  # add other variables in output here
}
data<-rbind(data,file.data)

nc_close(x)
}

