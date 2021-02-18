############################################################
##    REGRESSION ANALYSIS AND MAP   ########################
############################################################

# Create rasterstack of climate data for WRST (NOAA ClimGrid)
# Run linear regression analysis by pixel


rm(list = ls())

# ---   INITIALS  ---------------------------------------- #

data.dir <- "D:/nClimGrid" 
site = "WRST"

# read in parks shapefile
nps_boundary <- st_read('./data/spatial-data/nps_boundary')

park <- filter(nps_boundary, UNIT_CODE == site) # subset to WRST only
Sp_park <- as_Spatial(park) # park <- st_transform(park, st_crs(epsg))

bbox<-data.frame(Sp_park@bbox) # get bounding box

# Read in climate data

pnt.list<-list.files(path=data.dir, pattern=".prcp.alaska") #list all files by var

# Create list of tables

tables <- list()

for(i in 1:length(pnt.list)){
  t = read.table(paste(data.dir, pnt.list[i], sep = '/'))
  colnames(t) = c("Lat","Lon","PrecipC")
  tt = subset(t, Lat >= bbox["y","min"] & Lat <= bbox["y","max"] &
                 Lon >=bbox["x","min"] & Lon<=bbox["x","max"])
  tables[[i]] = tt 
}

# Create raster list

rasters <- list()

for(i in 1:length(tables)) {
  df <- tables[[i]]
  coordinates(df) = ~Lon+Lat
  proj4string(df) = "+proj=longlat +datum=WGS84 +no_defs " #same proj4string used in NPS_boundary_centroids.shp
  df = spTransform(df, CRSobj = "+init=epsg:3338") #reproj sp obj
  y = data.frame(df@coords)
  y$PrecipC<-df$PrecipC
  df = as.matrix(y)
  e = extent(df[,1:2])
  r =  raster(e, ncol=85, nrow=71)
  x = rasterize(df[, 1:2], r, df[,3])
  rasters[[i]] <- x
}

# Create raster stack

st <- stack(rasters)
  







r <- raster(e, ncol=85, nrow=71)
x <- rasterize(Dfx[, 1:2], r, Dfx[,3])
plot(x)

projection(x) = CRS("+init=epsg:3338")

# Plot park over raster
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:3338")


