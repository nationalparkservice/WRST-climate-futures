# WRST variable creation script
# Date: 210621
# Creator: A. Runyon

library(ncdf4)
library(reshape2)
library(WriteXLS)
library(data.table)
library(zoo)

rm(list=ls())

Out.dir <- "C:/Users/achildress/DOI/NPS-WRST-Resource Stewardship Strategy - Climate/Climate futures development/Data/"
load(paste0(Out.dir,"WRST_centroid","_init_parsed.RData"))

#Year range for summarizing future climate (Year - Range/2) to (Year + Range/2)
CF.mean = 2040 #Central year
Range = 30  #Number of years to summarize (should be at least 30)

# Threshold percentages for defining Climate futures. Default low/high:  0.25, 0.75
CFLow = 0.25     
CFHigh = 0.75

QuantileLow = 0.05   #Quantiles for temperature threshold calculations
QuantileHigh = 0.95

#Month and season names 
months=factor(c("January","February","March","April","May","June","July","August","September","October","November","December"),levels = month.name)

####################################
# Create dfs
head(daily)
head(water.balance)

daily$GCM <- paste(daily$GCM,daily$rcp,sep=".")

daily$TmaxCustom <- (daily$tmax * 9/5) + 32
daily$TminCustom <- (daily$tmax * 9/5) + 32
daily$TmeanCustom <- (daily$TmaxCustom + daily$TminCustom) / 2
daily$PrecipCustom <- daily$pcp / 25.4

daily$Year <- format(as.Date(daily$Date, format="%Y-%m-%d"),"%Y")
daily$Month <- as.numeric(format(as.Date(daily$Date, format="%Y-%m-%d"),"%m"))

Baseline_all <- subset(daily, Year < 2000)
Future_all <- subset(daily, Year >= CF.mean - (Range/2) & Year <= (CF.mean + (Range/2)))

# Baseline_Means
GCMs <- unlist(unique(daily$GCM))
Baseline_Means <- data.frame(GCM = GCMs)
Baseline_Means$DJF_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Baseline_all,Month %in% c(1,2,12)),mean)[,2]
Baseline_Means$MAM_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Baseline_all,Month %in% c(3,4,5)),mean)[,2]
Baseline_Means$JJA_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Baseline_all,Month %in% c(6,7,8)),mean)[,2]
Baseline_Means$SON_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Baseline_all,Month %in% c(9,10,11)),mean)[,2]

Baseline_Means$DJF_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Baseline_all,Month %in% c(1,2,12)),mean)[,2]*30
Baseline_Means$MAM_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Baseline_all,Month %in% c(3,4,5)),mean)[,2]*30
Baseline_Means$JJA_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Baseline_all,Month %in% c(6,7,8)),mean)[,2]*30
Baseline_Means$SON_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Baseline_all,Month %in% c(9,10,11)),mean)[,2]*30

# Format water.balance df
water.balance$Year <- format(as.Date(water.balance$Date, format="%Y-%m-%d"),"%Y")
water.balance$Month <- as.numeric(format(as.Date(water.balance$Date, format="%Y-%m-%d"),"%m"))


Baseline_WB <- subset(water.balance, Year < 2000)
Future_WB <- subset(water.balance, Year >= CF.mean - (Range/2) & Year <= (CF.mean + (Range/2)))

#Snow depth -- max water.balance$SWE
  # Annual max SWE
Annual_max_SWE <- aggregate(SWE~GCM+Year,Baseline_WB,max)
Baseline_Means$Annual_max_SWE <- aggregate(SWE~GCM,Annual_max_SWE,mean)[,2]

#Shoulder season snow -- sum MAM + SON SWE
Baseline_Means$MAM.SON_SWE <- aggregate(SWE~GCM,data=subset(Baseline_WB,Month %in% c(3,4,5,9,10,11)),mean)[,2]

# Format water.flux df
water.flux$Year <- format(as.Date(water.flux$Date, format="%Y-%m-%d"),"%Y")
water.flux$Month <- as.numeric(format(as.Date(water.flux$Date, format="%Y-%m-%d"),"%m"))
water.flux$WATER_BALANCE <- water.flux$PRCP - water.flux$EVAP


Baseline_WF <- subset(water.flux, Year < 2000)
Future_WF <- subset(water.flux, Year >= CF.mean - (Range/2) & Year <= (CF.mean + (Range/2)))

#Water balance -- precip - AET
Baseline_Means$water_balance <- aggregate(WATER_BALANCE~GCM,Baseline_WF,mean)[,2]


# Future_Means
Future_Means <- data.frame(GCM = GCMs)
Future_Means$DJF_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Future_all,Month %in% c(1,2,12)),mean)[,2]
Future_Means$MAM_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Future_all,Month %in% c(3,4,5)),mean)[,2]
Future_Means$JJA_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Future_all,Month %in% c(6,7,8)),mean)[,2]
Future_Means$SON_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Future_all,Month %in% c(9,10,11)),mean)[,2]

Future_Means$DJF_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Future_all,Month %in% c(1,2,12)),mean)[,2]*30
Future_Means$MAM_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Future_all,Month %in% c(3,4,5)),mean)[,2]*30
Future_Means$JJA_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Future_all,Month %in% c(6,7,8)),mean)[,2]*30
Future_Means$SON_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Future_all,Month %in% c(9,10,11)),mean)[,2]*30

#Snow depth -- max water.balance$SWE
# Annual max SWE
Annual_max_SWE <- aggregate(SWE~GCM+Year,Future_WB,max)
Future_Means$Annual_max_SWE <- aggregate(SWE~GCM,Annual_max_SWE,mean)[,2]

#Shoulder season snow -- sum MAM + SON SWE
Future_Means$MAM.SON_SWE <- aggregate(SWE~GCM,data=subset(Future_WB,Month %in% c(3,4,5,9,10,11)),mean)[,2]

#Water balance -- precip - AET
Future_Means$water_balance <- aggregate(WATER_BALANCE~GCM,Future_WF,mean)[,2]


## T1 Deltas
T1_Deltas <- Baseline_Means
T1_Deltas[,2:length(T1_Deltas)] <- Future_Means[,2:length(Future_Means)] - Baseline_Means[,2:length(Baseline_Means)]
