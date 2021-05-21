library(ncdf4)
library(reshape2)

rm(list=ls())

# MET parsing
data.dir<-"C:/Users/achildress/Documents/NCAR-test/met/"

x<-nc_open(paste0(data.dir,"ACCESS1-3_rcp45_BCSD_met_1950.nc4"))
x$dim$x


Lat = 33.791868
Lon = -80.748665
cLon = Lon + 360 #Adjusts negative lon. values 

###Parameters for extracting data from NetCDF files
#Specify top-level directory where MACA data is stored (all scenarios and variables)
MACADir = "E:/ClimateData/MACAv2Metdata" #no '/' at the end
setwd("C:/Users/adillon/Documents/RSS/CONG/MACA")

#Variable and scenario names corresponding to MACA data directory structure
vars = c("pr", "tasmax", "tasmin")
scens = c("historical", "rcp45", "rcp85")

#Variable names for output tables
VarNames = c("PrecipCustom", "TmaxCustom", "TminCustom","RHmaxCustom","RHminCustom")

# GCMs to be extracted
GCMs = c('bcc-csm1-1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
         'GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC365','HadGEM2-ES365',
         'inmcm4','IPSL-CM5A-MR','IPSL-CM5A-LR','IPSL-CM5B-LR',
         'MIROC5','MIROC-ESM','MIROC-ESM-CHEM','MRI-CGCM3','NorESM1-M')

#Date ranges to be extracted
Future_StartYear = 2006   #2006-2099
Future_EndYear = 2099   #2006-2099
Hist_StartYear = 1950     #1950-2005
Hist_EndYear = 2005      #1950-2005
HistYears = 50  

#Directory where output tables will be saved
OutDir = "C:/Users/adillon/Documents/RSS/CONG/MACA"





