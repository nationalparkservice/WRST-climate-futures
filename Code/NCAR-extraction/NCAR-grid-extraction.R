library(stars)
library(dplyr)
library('maptools')
library(ggplot2) #for plotting
library(units) # for dropping units
library(tidyverse)
library(starsdata)
library(viridis)


rm(list=ls())

#Lat = 62.23494 #park centroid lat
#Lon = -142.572 #park centroid lon
#cLon = Lon + 360 #Adjusts negative lon. values 


# MET parsing
data.dir<-"./data/met/" #location data file

######## Method working with .nc object and 'flattening' 
#x<-nc_open(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4")) 

#lon <- ncvar_get(x, "longitude") # read in lon
#lat <- ncvar_get(x, "latitude")
#tmax <- ncvar_get(x, "tmax") #working w/ tmax b/c pcp returns too many 0s


####### Stars method

# Can set stars objs as curvilinear -- couldn't figure out how to subset data

# Example from https://r-spatial.github.io/stars/articles/stars4.html#curvilinear-grids-1

#install.packages("starsdata", repos = "http://pebesma.staff.ifgi.de", type = "source") # Example curvilinear data

# --- EXAMPLE DATA (curvilinear) ------------------------------------------------------------ #

(s5p = system.file("sentinel5p/S5P_NRTI_L2__NO2____20180717T120113_20180717T120613_03932_01_010002_20180717T125231.nc", package = "starsdata")) # sample data

nit.c = read_stars(s5p, sub = "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_summed_total_column",
                   curvilinear = c("//PRODUCT/longitude", "//PRODUCT/latitude"), driver = NULL)

if (inherits(nit.c[[1]], "units")) {
  threshold = units::set_units(9e+36, mol/m^2)
} else {
  threshold = 9e+36
}
nit.c[[1]][nit.c[[1]] > threshold] = NA
st_crs(nit.c) = 4326
nit.c

# --------- END EXAMPLE DATA ------------------------------------------------------------------------------------------ #

s<-read_stars(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"), sub = 'tmax', curvilinear = c("longitude", "latitude"))

st_crs(s) = 4326
s

plot(s)

# plot arrays by time 

t1 <- s[,,,1] # time 1 
t2 <- s[,,,2] # time 2
t100 <- s[,,,100] # time 100

subset_x <- s[1,]

plot(t100)

test <- filter(s, slice_head) # need to expand grid to 62491 to use this filtering method. Maybe see how Amber did it in her code. 

prod(dim(s)) # 62491
st_dimensions(s)
x_dims <- st_get_dimension_values(s, 'x', max = TRUE)

test <- st_redimension(s, new_dims = prod(dim(s))) # error: summary operation prod not allowed 

# Look at ncdf file 

x<-nc_open(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"))

x

# Amber's example: plotting 

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
  scale_fill_viridis() +
  theme(panel.border = element_rect(colour = "black", fill = NA))

# According to https://r-spatial.org/r/2018/03/22/stars2.html - this allows you to subset stars obj. Didn't work for me
pol <- sst %>% st_bbox() %>% st_as_sfc() %>% st_centroid() %>% st_buffer(10) # I think this only works with regular grids, not curvilinear. Or maybe would work after being redimensioned
sst2 <- sst[,,1]
plot(sst[pol])

plot(sst)

## How extract data from sst?
s<-as.data.frame(sst[[1]]) # creates df from tmax. When looking @ df can see that only need to identify cells to extract from grid. Need index for lat/lon
lat.matrix<-as.data.frame(drop_units(cf[[1]])) #using library(units)
lat.right<-lat.matrix-Lat #latitude matrix - 0 -- hoping to identify id closest to 0
which.min(abs(as.numeric(unlist(lat.right))))

lon.matrix<-as.data.frame(cf[[2]])

## Next step - try converting park_centroid.shp to st obj and using it to extract cell?


# ----- HAVEN'T TRIED THIS ------------------------------------------------ #

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
