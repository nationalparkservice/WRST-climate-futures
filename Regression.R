############################################################
##    REGRESSION ANALYSIS AND MAP   ########################
############################################################

# Create rasterstack of climate data for WRST (NOAA ClimGrid)
# Run linear regression analysis by pixel
# Units for raw data: temps in Celsius, precip in mm


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

# ----  CREATE RASTER STACKS  ----------------------------- #

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

st <- stack(rasters) # Create raster stack

# Summarize by year

index <- rep(1:96, each = 12) # Because data is provided monthly, need to assign index by year. There are 96 years in the dataset.

precip = function(x){ # Function for calculating mean annual precip from mm to inches
  (x*12)/25.4
}
  
st_pr_mean <- stackApply(st, indices = index, fun = mean, na.rm = TRUE) # get annual mean first
st_pr_yr <- calc(st_pr_mean, fun = precip) # then convert to inches

# ------  REGRESSION  -------------------------------------------------- #
  
time <- 1:nlayers(st_pr_yr) # all years 1925 - 2020

# Function to calculate slope and p-value

fun <- function(y) {
  if(all(is.na(y))) {
    c(NA, NA)
  } else {
    m = lm(y ~ time) 
    s = summary(m)
    slope = s$coefficients[2]
    pval =  pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],lower.tail = FALSE)
    cbind(slope, pval)
  }
}

r <- calc(st_pr_yr, fun)




# Plot park over raster
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:3338")


