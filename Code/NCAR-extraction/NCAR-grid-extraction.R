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

####### Stars method
# Can set stars objs as curvilinear -- couldn't figure out how to subset data
# s<-read_ncdf(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"), curvilinear = c("longitude", "latitude"),var="tmax")


# Try example from https://www.mattreusswig.com/post/use-stars-to-visualize-curvilinear-netcdf-rasters/
sst <- read_ncdf(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"), var = c("tmax"))
cf  <- read_ncdf(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"), var = c("latitude", "longitude"))
sst
cf

sst <- read_ncdf(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"), var = c("tmax"), 
                 ## Specify the layers to read in by dimension. Dimension 3 corresponds to lake 
                 ## depth with 10 layers. We'll use layer 10 since its at the lake bottom.
                 ## Dimension 4 is time in days (listed as an index from 1 to 365). We'll grab
                 ## day 1 and read it into memory.
                 ncsub = cbind(start = c(1, 1, 200), count = c(299, 209, 1)))
plot(sst[1])
plot(cf[1])
plot(cf[2])

x = matrix(cf[[2]], 299, 209)
y = matrix(cf[[1]], 299, 209)

sst <- st_as_stars(sst,
                   curvilinear = list(x = x, y = y))

sst

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
st_crs(sst) <- "+proj=longlat +ellps=WGS84 +no_defs"
sst <- st_transform(sst, st_crs(world))

attributes(sst)$dimensions[1]


ggplot()  + 
  geom_sf(data = world, color = "black", fill = "white") +
  geom_stars(data = sst, alpha = 0.75) +
  coord_sf(ylim = c(min(cf$latitude),max(cf$latitude)), xlim = c(min(cf$longitude),max(cf$longitude))) +
  # geom_point(data=Sp_park) +
  scale_fill_viridis() +
  theme(panel.border = element_rect(colour = "black", fill = NA))

# According to https://r-spatial.org/r/2018/03/22/stars2.html - this allows you to subset stars obj. Didn't work for me
pol <- sst %>% st_bbox() %>% st_as_sfc() %>% st_centroid() %>% st_buffer(10)
sst <- sst[,,1]
plot(sst[pol])

## How extract data from sst?
s<-as.data.frame(sst[[1]]) # creates df from tmax. When looking @ df can see that only need to identify cells to extract from grid. Need index for lat/lon
lat.matrix<-as.data.frame(drop_units(cf[[1]])) #using library(units)
lat.right<-lat.matrix-Lat #latitude matrix - 0 -- hoping to identify id closest to 0
which.min(abs(as.numeric(unlist(lat.right))))

lon.matrix<-as.data.frame(cf[[2]])

## Next step - try converting park_centroid.shp to st obj and using it to extract cell?
St_park<-st_as_sf(park[1,])
crs(St_park) <- "+proj=longlat +datum=WGS84 +no_defs"
st_ext<-st_extract(sst, St_park) #doesn't work 

# Curvilinear subset from https://r-spatial.github.io/stars/articles/stars1.html
library(dplyr) # loads slice generic
prec_slice = slice(prec, index = 17, along = "time")
plot(prec_slice, border = NA, breaks = qu_0_omit(prec_slice[[1]]), reset = FALSE)
nc = sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg")
plot(st_geometry(nc), add = TRUE, reset = FALSE, col = NA, border = 'red')

##### RASTER OPTION


# Haven't made much headway here can just read in netcdf as a brick and plot it
y = brick(ncvar_get(x, "tmax"))
crs(y)
plot(y[[200]]) #change # and can see that it's changing the date, which defines the layers
z=stack(y)
plot(z[[1]])

crs(Sp_park) <-"+init=epsg:3338"
crs(z) <- "+init=epsg:3338"

r<-mask(y,Sp_park) #subsets but cells are NA, indicating it's wrong cell

bbox<-data.frame(Sp_park@bbox)
bbox[1,]<-bbox[1,] + 365
dfx<-subset(, Lat >= bbox["y","min"] & Lat <= bbox["y","max"] &
              Lon >=bbox["x","min"] & Lon<=bbox["x","max"]) #doesn't work with small parks, need to fix


y.raster<-raster(y[[200]]) # can't figure out how to extract single raster from brick


# None of this shit worked
crs(y) <- "+proj=longlat +datum=WGS84 +no_defs"
# crs(y) <- "+init=epsg:3338"
# projectRaster(y,crs = "+init=epsg:3338")
# 
# rast.lon<-raster(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"),varname="longitude")
# y2<-projectRaster(y[[1]],crs = "+init=epsg:3338") #reproj sp obj
# y[[1]]
