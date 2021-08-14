rm(list = ls())

library(stars);library(dplyr);library(ggplot2);library(ggthemes);library(viridis);library(here);library(ggrepel);library(rlang);library(units)

areas <- c('wrst_mtns', 'copper_plateau', 'coastal_mtns', 'eastern_coast', 'WRST_simple')


met.dir <- "D:/NCAR_AK/met" 
vic.dir <- "D:/NCAR_AK/vic_hydro"

working.dir <- "C:/Users/achildress/Documents/wrst_temp"
boundary.dir <- "C:/Users/achildress/DOI/NPS-WRST-Resource Stewardship Strategy - Climate/3.0 Climate futures development (Summer 2021)/Data/Boundary_shapefiles/"

historical.period <- as.character(seq(1950,1999,1))
future.period <- as.character(seq(2025,2055,1))
daymet.period <- as.character(seq(1980,2016,1))

GCMs <- c("inmcm4.rcp85","ACCESS1-3.rcp45","CanESM2.rcp85")
CFs <- c("Climate Future 1", "Climate Future 2", "Climate Future 3")
cols <- c("#12045C","#ffcccc","#E10720")

CF_GCM <- data.frame(CF=CFs,GCM=GCMs,CF_col=cols)
                     
for (a in 1:length(areas)){
  area = areas[a]
  
  shp <- st_read(paste0(boundary.dir, area, ".shp")) # Wrangell Mountains
  shp <- st_transform(shp, 3338)
  
  data.dir <- paste(working.dir, "Data", area, sep="/")
  
  dir.create(data.dir,showWarnings=FALSE)
  
  gc()
  memory.limit(60000)
  source(here::here("Code", "Metric-development", "met","met-daily-stars_by_year.R"),echo = FALSE) 
  source(here::here("Code", "Metric-development", "vic","SWE-runoff-Pr99.R"),echo = FALSE)
  
}
