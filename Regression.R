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
test <- list()

for(i in 1:length(pnt.list)){
  t = read.table(paste(data.dir, pnt.list[i], sep = '/'))
  colnames(t) = c("Lat","Lon","PrecipC")
  tt = subset(t, Lat >= bbox["y","min"] & Lat <= bbox["y","max"] &
                 Lon >=bbox["x","min"] & Lon<=bbox["x","max"])
  tables[[i]] = tt 
}



