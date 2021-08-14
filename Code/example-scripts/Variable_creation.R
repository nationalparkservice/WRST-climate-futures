# WRST variable creation script
# Date: 210621
# Creator: A. Runyon

library(ncdf4)
library(reshape2)
library(WriteXLS)
library(data.table)
library(zoo)
library(cowplot)
library(ggbiplot)
library(ggplot2)
library(ggrepel)
library(tidyverse)

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

## Convert to tidy
daily <- as_tibble(daily)
daily <- daily %>% unite("GCM", GCM, rcp, sep=".", remove = FALSE)

daily <- daily %>%
  mutate(TmaxCustom = (tmax *9/5) +32) %>%
  mutate(TminCustom = (tmin *9/5) +32) %>%
  mutate(TmeanCustom = (TmaxCustom + TminCustom) / 2) %>%
  mutate(PrecipCustom = pcp / 25.4) 

daily$Year <- format(as.Date(daily$Date, format="%Y-%m-%d"),"%Y")
daily$Month <- as.numeric(format(as.Date(daily$Date, format="%Y-%m-%d"),"%m"))

Baseline_all <- daily %>% filter(Year < 2000)
Future_all <- daily %>% filter( Year >= CF.mean -  (Range/2) & Year <= (CF.mean + (Range/2)))


# Baseline_Means
GCMs <- unlist(unique(daily$GCM))
Baseline_Means <- tibble(GCM = GCMs)
Baseline_Means

# Baseline_Means$DJF_Tmean_F <- # Same as aggregate but longer -- what is advantage?
#   (Baseline_all %>% filter(Month %in% c(1,2,12)) %>% group_by(GCM) %>% summarize(mean = mean(TmeanCustom)))[,2]

Baseline_Means$DJF_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Baseline_all,Month %in% c(1,2,12)),mean)[,2]
Baseline_Means$MAM_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Baseline_all,Month %in% c(3,4,5)),mean)[,2]
Baseline_Means$JJA_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Baseline_all,Month %in% c(6,7,8)),mean)[,2]
Baseline_Means$SON_Tmean_F <- aggregate(TmeanCustom~GCM,data=subset(Baseline_all,Month %in% c(9,10,11)),mean)[,2]

Baseline_Means$DJF_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Baseline_all,Month %in% c(1,2,12)),mean)[,2]*30
Baseline_Means$MAM_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Baseline_all,Month %in% c(3,4,5)),mean)[,2]*30
Baseline_Means$JJA_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Baseline_all,Month %in% c(6,7,8)),mean)[,2]*30
Baseline_Means$SON_Precip_in <- aggregate(PrecipCustom~GCM,data=subset(Baseline_all,Month %in% c(9,10,11)),mean)[,2]*30

Baseline_Means$Tmean_F <- aggregate(TmeanCustom~GCM,data=Baseline_all,mean)[,2]
Baseline_Means$Precip_in <- aggregate(PrecipCustom~GCM,data=Baseline_all,mean)[,2]*365

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

Future_Means$Tmean_F <- aggregate(TmeanCustom~GCM,data=Future_all,mean)[,2]
Future_Means$Precip_in <- aggregate(PrecipCustom~GCM,data=Future_all,mean)[,2]*365

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

####################### PCA TO SELECT TWO MODELS
head(T1_Deltas)
T1_rownames <- T1_Deltas[,-1]
rownames(T1_rownames) <- T1_Deltas[,1]

WRST.pca <- prcomp(T1_rownames[,c(1:11)], center = TRUE,scale. = TRUE)

head(WRST.pca$rotation)
head(WRST.pca$x)
summary(WRST.pca)

str(WRST.pca)


ggbiplot(WRST.pca, labels=rownames(T1_rownames))

WRST.pca.x<-as.data.frame(WRST.pca$x)

# This method doesn't return the exact models we selected,
# but we may not have selected the most divergent models
rownames(WRST.pca.x)[which.min(WRST.pca.x$PC1)]
rownames(WRST.pca.x)[which.max(WICA.pca.x$PC1)]
rownames(WRST.pca.x)[which.min(WRST.pca.x$PC2)]
rownames(WRST.pca.x)[which.max(WRST.pca.x$PC2)]

# Scatterplots
#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=22),                                                                   #Text size of legend title
                  legend.position = "bottom")                                                                            #Legend position


head(T1_Deltas)
Longx<- "annual average temperature (F)"
Longy<- "annual average precipitation (in)"
x <- "Tmean_F"
y <- "Precip_in"

xvar= T1_Deltas$Tmean_F
yvar= T1_Deltas$Precip_in

xvar25 = as.numeric(quantile(xvar, .25))
xvaravg = as.numeric(mean(xvar))
xvar75 = as.numeric(quantile(xvar, .75))

yvar25 = as.numeric(quantile(yvar, .25))
yvaravg = as.numeric(mean(yvar))
yvar75 = as.numeric(quantile(yvar, .75))

## Color only scenarios using

dualscatter = ggplot(T1_Deltas, aes(xvar, yvar,col,  xmin=xvar25, xmax=xvar75, ymin=yvar25, ymax=yvar75))
dualscatter  + geom_text_repel(hjust=0,vjust=0,aes(label=GCM))  +
  geom_point(size=4) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(yvar)),linetype=2) + 
  geom_vline(aes(xintercept=mean(xvar)),linetype=2) + 
  labs(title =paste("WRST Changes in climate means in 2040 by GCM run\n", Longx," vs. ",Longy,sep=""), 
       x = paste("Changes in ",Longx,sep=""), 
       y = paste("Changes in ",Longy,sep=""))




Longx<- "annual max SWE (mm)"
Longy<- "annual water balance (mm)"
x <- "Annual_max_SWE"
y <- "water_balance"

xvar= T1_Deltas$Annual_max_SWE
yvar= T1_Deltas$water_balance

xvar25 = as.numeric(quantile(xvar, .25))
xvaravg = as.numeric(mean(xvar))
xvar75 = as.numeric(quantile(xvar, .75))

yvar25 = as.numeric(quantile(yvar, .25))
yvaravg = as.numeric(mean(yvar))
yvar75 = as.numeric(quantile(yvar, .75))

## Color only scenarios using

dualscatter = ggplot(T1_Deltas, aes(xvar, yvar,col,  xmin=xvar25, xmax=xvar75, ymin=yvar25, ymax=yvar75))
dualscatter  + geom_text_repel(hjust=0,vjust=0,aes(label=GCM))  +
  geom_point(size=4) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(yvar)),linetype=2) + 
  geom_vline(aes(xintercept=mean(xvar)),linetype=2) + 
  labs(title =paste("WRST Changes in climate means in 2040 by GCM run\n", Longx," vs. ",Longy,sep=""), 
       x = paste("Changes in ",Longx,sep=""), 
       y = paste("Changes in ",Longy,sep=""))



Longx<- "shoulder season snow (mm)"
Longy<- "summer precip (in)"
x <- "MAM.SON_SWE"
y <- "JJA_Precip_in"

xvar= T1_Deltas$MAM.SON_SWE
yvar= T1_Deltas$JJA_Precip_in

xvar25 = as.numeric(quantile(xvar, .25))
xvaravg = as.numeric(mean(xvar))
xvar75 = as.numeric(quantile(xvar, .75))

yvar25 = as.numeric(quantile(yvar, .25))
yvaravg = as.numeric(mean(yvar))
yvar75 = as.numeric(quantile(yvar, .75))

## Color only scenarios using

dualscatter = ggplot(T1_Deltas, aes(xvar, yvar,col,  xmin=xvar25, xmax=xvar75, ymin=yvar25, ymax=yvar75))
dualscatter  + geom_text_repel(hjust=0,vjust=0,aes(label=GCM))  +
  geom_point(size=4) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(yvar)),linetype=2) + 
  geom_vline(aes(xintercept=mean(xvar)),linetype=2) + 
  labs(title =paste("WRST Changes in climate means in 2040 by GCM run\n", Longx," vs. ",Longy,sep=""), 
       x = paste("Changes in ",Longx,sep=""), 
       y = paste("Changes in ",Longy,sep=""))

