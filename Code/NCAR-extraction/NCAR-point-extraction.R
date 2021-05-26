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

Lat = 62.23494 #park centroid lat
Lon = -142.572 #park centroid lon
cLon = Lon + 360 #Adjusts negative lon. values 

# read in parks shapefile
nps_boundary <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids/nps_boundary_centroids.shp')
park <- filter(nps_boundary, UNIT_CODE == "WRST") # subset to WRST only
Sp_park <- as_Spatial(park[1,])


# MET parsing
data.dir<-"C:/Users/achildress/Documents/NCAR-test/met/" #location data file

######## Method working with .nc object and 'flattening' 
x<-nc_open(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4")) 

lon <- ncvar_get(x, "longitude") # read in lon
lat <- ncvar_get(x, "latitude")
tmax <- ncvar_get(x, "tmax") #working w/ tmax b/c pcp returns too many 0s
tmax[200,73,] #returns tmax data for lon 200,lat 73, all time (365)


# this chunk of code from Method 2, here https://gis.stackexchange.com/questions/390148/handle-curvilinear-rotated-grid-netcdf-file-in-r
# it flattens the grid by converting coordinate to vectors
names(x$var)
req_var<-names(x$var)[4] #tmax
ts<-as.POSIXct(nc.get.time.series(x))
dum_var <- ncvar_get(x, req_var,start=c(1,1,1),count = c(-1,-1,1)) 
coords<-data.frame(id=1:length(lon),lon=as.vector(lon), lat=as.vector(lat)) #index values match up to 

coords$lat.distance<-abs(coords$lat - Lat)
coords$lon.distance<-abs(coords$lon -cLon)

coords$index<-coords$lat.distance*coords$lon.distance

Index = coords$id[which.min(coords$index)]

Lat_index = as.numeric(which.min(abs(coords$lat - Lat))) #returns cell index that is closest to Lat var
Lon_index = as.numeric(which.min(abs(coords$lon -cLon)))#returns cell index that is closest to Lon var
# Note these don't work because data are a matrix, not index so don't identify same cell

data_out <- rep(0,length(ts)) #create zeros vector to output the average value

id <-c(Index) # c(Lat_index,Lon_index) # index of cells within the mask shapefile -- single value if single cell, multi values if multi cells

for (i in 1:length(id)) {
  
  pt_id <- id[i]
  #convert linear ind to r and c index
  rc <- arrayInd(pt_id,dim(dum_var)) #get row and column index
  
  #get the variable at the required location for all time steps.
  
  pt_data <- ncvar_get(x, req_var,  
                       start=c(rc[1],rc[2],1),count=c(1,1,-1)) #-1 read all time_steps
  data_out <- data_out+pt_data #sum data to be averaged after the loop ## Doesn't work if NA values
}
data_out

nc_close(x)

