############################################################
##    REGRESSION ANALYSIS AND MAP   ########################
############################################################

# Create rasterstack of climate data for WRST (NOAA ClimGrid)
# Run linear regression analysis by pixel
# Units for raw data: temps in Celsius, precip in mm

library(sf)
library(sp)
library(dplyr)
library(raster)
library(rasterVis)

rm(list = ls())

# ---   USER INPUTS -------------------------------------- #

data.dir <- "D:/nClimGrid" 
pnt.list<-list.files(path=data.dir, pattern=".prcp.alaska") #list all files by var
var <- "pr"


# ---   INITIALS  ---------------------------------------- #

site = "WRST"

# read in parks shapefile
nps_boundary <- st_read('./data/spatial-data/nps_boundary')

park <- filter(nps_boundary, UNIT_CODE == site) # subset to WRST only
Sp_park <- as_Spatial(park) # park <- st_transform(park, st_crs(epsg))

bbox<-data.frame(Sp_park@bbox) # get bounding box

out <- './output' 
if(dir.exists(out) == FALSE){
  dir.create(out)
}

maps <- './output/maps' 
if(dir.exists(maps) == FALSE){
  dir.create(maps)
}

ras <- './output/rasters' 
if(dir.exists(ras) == FALSE){
  dir.create(ras)
}

# ----  CREATE RASTER STACKS  ----------------------------- #

# Create list of tables

tables <- list()

for(i in 1:length(pnt.list)){
  t = read.table(paste(data.dir, pnt.list[i], sep = '/'))
  colnames(t) = c("Lat","Lon", var)
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
  y$var<-df@data
  df = as.matrix(y)
  e = extent(df[,1:2])
  r =  raster(e, ncol=85, nrow=71)
  x = rasterize(df[, 1:2], r, df[,3])
  rasters[[i]] <- x
}

st <- stack(rasters) # Create raster stack

index <- rep(1:96, each = 12)

# Summarize by year

# Calculate annual means: output = rasterstack with 1 layer per year

st_mean <- stackApply(st, indices = c(rep(1:96, each = 12)), fun = mean, na.rm = TRUE) # get annual mean first

st_sum <- stackApply(st, indices = c(rep(1:96, each = 12)), fun = sum, na.rm = TRUE) # get total annual precip
plot(st_sum[[1]])


st_mean_pr_tot <- calc(st_sum, fun = mean)
plot(st_mean_pr_tot)


st_fahr <- calc(st_mean, fun = function(x){x*9/5 + 32}) # then convert to Fahrenheit

st_sum_in <- calc(st_sum, fun = function(x){((x)/25.4)})
plot(st_in)


# Calculate overall mean: output = single raster with overall mean values

ras_mean <- calc(st_in, fun = mean)
plot(ras_mean)

png('mean_total_precip_mm.png')
plot(st_mean_pr_tot)
dev.off()

# ------  REGRESSION  -------------------------------------------------- #
  
time <- 1:nlayers(st_sum_in) # all years 1925 - 2020

# Function to calculate slope and p-value

fun <- function(y) {
  if(all(is.na(y))) {
    c(NA, NA)
  } else {
    m = lm(y ~ time) 
    s = summary(m)
    slope = s$coefficients[2] * 100 # change per 100 years
    pval =  pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],lower.tail = FALSE)
    cbind(slope, pval)
  }
}

r <- calc(st_sum_in, fun)
plot(r)

# -- PLOTTING ---------------------------------------------------------- #

# Reclassify raster so that values <= 0.05 -> 1 or else NA
slope <- subset(r, 1)

pval <- subset(r, 2)
pval[pval > 0.05] <- NA # Make values NA that are greater than 0.05

sig <- mask(slope, pval)

plot(sig)

# Plot park over raster
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:3338") # project to Alaska Albers
plot(Sp_park, add = TRUE)


writeRaster(sig, file = "./output/rasters/precip_delta.tif") # save raster of significant slope values






