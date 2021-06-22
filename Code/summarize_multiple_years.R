#############################################################################
###   SUMMARIZING ACROSS MULTIPLE YEARS   ###################################
#############################################################################


library(sf)
library(stars)
library(dplyr)
library(tidyr)
library(ncmeta)
library(ggplot2)
library(ggthemes)
library(viridis)


rm(list = ls())

# ------- Read in data from multiple files  ----------------------- #

# Example using read_stars() with two files

x = c(
  "C:/Users/adillon/Documents/RSS/WRST/Test/met/Historical/MIROC5_rcp45_BCSD_met_1950.nc4",
  "C:/Users/adillon/Documents/RSS/WRST/Test/met/Historical/MIROC5_rcp45_BCSD_met_1951.nc4"
)

y <- read_stars(x, sub = "tmax") # This works but result loses its spatial reference info


ncdf <- list()

for(i in 1:length(x)){
  ncdf[[i]] <- read_ncdf(x[i], var = "tmax", curvilinear = c("longitude", "latitude"))
}

test <- c(ncdf[[1]], ncdf[[2]]) # this works but I don't know how to do it with n numbers

test <- Reduce(c, ncdf)

test2 <- st_as_stars()
write_stars(test2, "test2.nc4")



write_stars(test2, dsn = './output', normalize_path = TRUE)


file_list <- dir(path = "C:/Users/adillon/Documents/RSS/WRST/Test/met/Historical", pattern = '.nc4', all.files = TRUE, full.names = TRUE) # MIROC5 rcp45

x <- read_ncdf(file_list, curvilinear = c("longitude", "latitude"))


tmax <- x %>% select(tmax) # select tmax only 

tmax
