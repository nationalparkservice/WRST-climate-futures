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
Year = 2040 #Central year
Range = 30  #Number of years to summarize (should be at least 30)

# Threshold percentages for defining Climate futures. Default low/high:  0.25, 0.75
CFLow = 0.25     
CFHigh = 0.75

QuantileLow = 0.05   #Quantiles for temperature threshold calculations
QuantileHigh = 0.95

#Month and season names 
months=factor(c("January","February","March","April","May","June","July","August","September","October","November","December"),levels = month.name)
seasons=factor(c("Winter", "Spring", "Summer", "Fall"))
levels(seasons)=seasons

####################################
# Create dfs
head(daily)
head(vic)

daily$TmaxCustom <- (daily$tmax * 9/5) + 32
daily$TminCustom <- (daily$tmax * 9/5) + 32
daily$TmeanCustom <- (daily$TmaxCustom + daily$TminCustom) / 2
daily$PrecipCustom <- daily$pcp / 25.4

daily$Year <- format(as.Date(daily$Date, format="%Y-%m-%d"),"%Y")

Baseline_all <- subset(daily, Year < 2000)
Future_all <- subset(daily, Year >= Year - (Range/2) & Year <= (Year + (Range/2)))

################################################### FUNCTION DEFINITIONS ########################################

#### FUNCTION TO CALCULATE SEASON FROM 'DATE' ####

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF TOTAL DAYS/YEAR FOR A VARIABLE ####

MeanAnnualTotals = function(DF, VarName){
  Years = length(unique(strftime(DF$Date, "%Y")))
  VarIndex = which(colnames(DF) == VarName)
  MeanAnnualTotal = aggregate(DF[,VarIndex] ~ DF$GCM, FUN=function(x){sum(x)/Years}) 
  names(MeanAnnualTotal) = c("GCM", "MeanAnnualTotals")
  return(MeanAnnualTotal)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF MAXIMUM ANNUAL DAYS/YEAR FOR A VARIABLE ####
MeanAnnualMax = function(DF, VarName){
  VarIndex = which(colnames(DF) == VarName)
  YearlyMax = aggregate(DF[,VarIndex], by=list(GCM=DF$GCM, Year=DF$Date$year), FUN=max)
  MeanAnnualMax = aggregate(YearlyMax[,3] ~ YearlyMax$GCM, FUN=mean)
  names(MeanAnnualMax) = c("GCM", "MeanAnnualMax")
  return(MeanAnnualMax)
}

#### END FUNCTION ####


#### FUNCTION TO COMPARE BASELINE TO FUTURE MEANS AND CALCULATE DELTAS ####

GetAnnualMeanDeltas = function(BaseMeans, FutureMeans){
  TotalMeans = merge(BaseMeans, FutureMeans, by="GCM")
  TotalMeans$Delta = unlist(FutureMeans[2] - BaseMeans[2])
  return(TotalMeans)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF MAXIMUM SEASONAL DAYS/YEAR FOR A VARIABLE ####
MeanSeasonalMax = function(DF, VarName){
  VarIndex = which(colnames(DF) == VarName)
  YearlyMax = aggregate(DF[,VarIndex], by=list(GCM=DF$GCM, Year=DF$Date$year, Season=DF$season), FUN=max)
  MeanAnnualMax = aggregate(YearlyMax[,4], by=list(GCM=YearlyMax$GCM, Season=YearlyMax$Season), FUN=mean)
  names(MeanAnnualMax) = c("GCM", "Season", "MeanAnnualMax")
  return(MeanAnnualMax)
}

#### END FUNCTION ####


#### FUNCTION TO COMPARE BASELINE TO FUTURE MEANS AND CALCULATE DELTAS ####

GetSeasonalMeanDeltas = function(BaseMeans, FutureMeans){
  TotalMeans = merge(BaseMeans, FutureMeans, by=c("GCM", "Season"))
  TotalMeans$Delta = unlist(FutureMeans[3] - BaseMeans[3])
  return(TotalMeans)
}

#### END FUNCTION ####

#### FUNCTION TO CALCULATE HEAT INDEX ####
#use daily tmax for temp as well as rhs_max
heat_index <- function(temp, RH) {
  Sted <- 0.5 * (temp + 61 + ((temp - 68) * 1.2) + (RH * 0.094))
  Roth <- -42.379 + (2.04901523 * temp) + (10.14333127 * RH) + (-.22475541 * temp * RH) +
    (-.00683783 * temp^2) + (-.05481717 * RH^2) + (.00122874 * temp^2 * RH) + 
    (.00085282 * temp * RH^2) + (-.00000199 * temp^2 * RH^2)
  adj1 <- ((13 - RH) / 4) * sqrt((17 - abs(temp - 95)) / 17)
  adj2 <- ((RH - 85) / 10) * ((87 - temp) / 5)
  heat_index<-ifelse(temp < 80, Sted, 
                     ifelse(RH < 13 & temp > 80 & temp < 112, Roth-adj1,
                            ifelse(RH > 85 & temp > 80 & temp < 87, Roth+adj2, Roth)))
  heat_index
} #creates errors but doesn't matter becuase not used when not applicable

#### END FUNCTION ####

################################ END FUNCTION DEFINITIONS #########################################

################################# SUMMARIZE CHANGE IN FUTURE TEMP/PRECIP MEANS BY GCM ####################
####Set Average values for all four weather variables, using all baseline years and all climate models
BaseMeanPr = mean(Baseline_all$PrecipCustom)
BaseMeanTmx = mean(Baseline_all$TmaxCustom)
BaseMeanTmn = mean(Baseline_all$TminCustom)

####Create Future/Baseline means data tables, with averages for all four weather variables, organized by GCM
Future_Means = data.frame(aggregate(cbind(Future_all$PrecipCustom, Future_all$TmaxCustom, Future_all$TminCustom)
                                    ~ Future_all$GCM, Future_all, mean,na.rm=F))   # , Future_all$Wind
names(Future_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")    # , "Wind"
Future_Means$TavgCustom = (Future_Means$TmaxCustom + Future_Means$TminCustom)/2

Baseline_Means = data.frame(aggregate(cbind(PrecipCustom, TmaxCustom, TminCustom)~GCM, 
                                      Baseline_all[which(Baseline_all$GCM %in% unique(Future_all$GCM)),], mean))    #  ,Baseline_all$Wind)
names(Baseline_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")  #  , "Wind")
Baseline_Means$TavgCustom = (Baseline_Means$TmaxCustom + Baseline_Means$TminCustom)/2

#### add delta columns in order to classify CFs
Future_Means$DeltaPr = Future_Means$PrecipCustom - Baseline_Means$PrecipCustom
Future_Means$DeltaTmx = Future_Means$TmaxCustom - Baseline_Means$TmaxCustom
Future_Means$DeltaTmn = Future_Means$TminCustom - Baseline_Means$TminCustom
Future_Means$DeltaTavg = Future_Means$TavgCustom - Baseline_Means$TavgCustom

#### Set limits for CF classification
Pr0 = as.numeric(quantile(Future_Means$DeltaPr, 0))
Pr25 = as.numeric(quantile(Future_Means$DeltaPr, CFLow))
PrAvg = as.numeric(mean(Future_Means$DeltaPr))
Pr75 = as.numeric(quantile(Future_Means$DeltaPr, CFHigh))
Pr100 = as.numeric(quantile(Future_Means$DeltaPr, 1))
Tavg0 = as.numeric(quantile(Future_Means$DeltaTavg, 0))
Tavg25 = as.numeric(quantile(Future_Means$DeltaTavg, CFLow)) 
Tavg = as.numeric(mean(Future_Means$DeltaTavg))
Tavg75 = as.numeric(quantile(Future_Means$DeltaTavg, CFHigh))
Tavg100 = as.numeric(quantile(Future_Means$DeltaTavg, 1))

#### Designate Climate Future
Future_Means$CF1 = as.numeric((Future_Means$DeltaTavg<Tavg & Future_Means$DeltaPr>Pr75) | Future_Means$DeltaTavg<Tavg25 & Future_Means$DeltaPr>PrAvg)
Future_Means$CF2 = as.numeric((Future_Means$DeltaTavg>Tavg & Future_Means$DeltaPr>Pr75) | Future_Means$DeltaTavg>Tavg75 & Future_Means$DeltaPr>PrAvg)
Future_Means$CF3 = as.numeric((Future_Means$DeltaTavg>Tavg25 & Future_Means$DeltaTavg<Tavg75) & (Future_Means$DeltaPr>Pr25 & Future_Means$DeltaPr<Pr75))
Future_Means$CF4 = as.numeric((Future_Means$DeltaTavg<Tavg & Future_Means$DeltaPr<Pr25) | Future_Means$DeltaTavg<Tavg25 & Future_Means$DeltaPr<PrAvg)
Future_Means$CF5 = as.numeric((Future_Means$DeltaTavg>Tavg & Future_Means$DeltaPr<Pr25) | Future_Means$DeltaTavg>Tavg75 & Future_Means$DeltaPr<PrAvg)

#Assign full name of climate future to new variable CF
Future_Means$CF[Future_Means$CF1==1]=CFs[1]
Future_Means$CF[Future_Means$CF2==1]=CFs[2]
Future_Means$CF[Future_Means$CF3==1]=CFs[3]
Future_Means$CF[Future_Means$CF4==1]=CFs[4]
Future_Means$CF[Future_Means$CF5==1]=CFs[5]
Future_Means$CF=as.factor(Future_Means$CF)
Future_Means$CF = factor(Future_Means$CF,ordered=TRUE,levels=CFs)

#     Remove extraneous Climate Future columns
Future_Means$CF1 = NULL
Future_Means$CF2 = NULL
Future_Means$CF3 = NULL
Future_Means$CF4 = NULL
Future_Means$CF5 = NULL

#     Add column with emissions scenario for each GCM run
Future_Means$emissions[grep("rcp85",Future_Means$GCM)] = "RCP 8.5"
Future_Means$emissions[grep("rcp45",Future_Means$GCM)] = "RCP 4.5"

####Add column with CF classification to Future_all/Baseline_all
CF_GCM = data.frame(GCM = Future_Means$GCM, CF = Future_Means$CF)
Future_all = merge(Future_all, CF_GCM[1:2], by="GCM")
Baseline_all$CF = "Historical"

################################ SUMMARIZE TEMPERATURE, PRECIP, RH BY MONTH & SEASON #######################
Baseline_all$Month<-format(Baseline_all$Date,"%m")
Baseline_all$Year<-format(Baseline_all$Date,"%Y")
Future_all$Month<-format(Future_all$Date,"%m")
Future_all$Year<-format(Future_all$Date,"%Y")

#Add season variable for all data in Future_all and Baseline_all
Future_all$season=getSeason(Future_all$Date)
Baseline_all$season=getSeason(Baseline_all$Date)

#### Create tables with monthly tmax/tmin/tmean/precip/RHmean delta by CF
# Historical
Tmax = aggregate(TmaxCustom~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Tmin = aggregate(TminCustom~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgCustom~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Precip = aggregate(PrecipCustom~Month+Year+GCM+CF,Baseline_all,sum,na.rm=TRUE)
Precip = aggregate(PrecipCustom~Month+GCM+CF,Precip,mean,na.rm=TRUE)
Baseline_all$RHmean<-(Baseline_all$RHmaxCustom+Baseline_all$RHminCustom)/2
RHmean = aggregate(RHmean~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)

H_Monthly<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean))
rm(Tmax,Tmin,Tmean,Precip,RHmean)

# Future
Tmax = aggregate(TmaxCustom~Month+GCM+CF,Future_all,mean,na.rm=TRUE)
Tmin = aggregate(TminCustom~Month+GCM+CF,Future_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgCustom~Month+GCM+CF,Future_all,mean,na.rm=TRUE)
Precip = aggregate(PrecipCustom~Month+Year+GCM+CF,Future_all,sum,na.rm=TRUE)
Precip = aggregate(PrecipCustom~Month+GCM+CF,Precip,mean,na.rm=TRUE)
Future_all$RHmean<-(Future_all$RHmaxCustom+Future_all$RHminCustom)/2
RHmean = aggregate(RHmean~Month+GCM+CF,Future_all,mean,na.rm=TRUE)

F_Monthly<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean))
rm(Tmax,Tmin,Tmean,Precip,RHmean)


#### Create tables with seasonal tmax/tmin/tmean/precip/RHmean delta by CF
# Historical
Tmax = aggregate(TmaxCustom~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Tmin = aggregate(TminCustom~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgCustom~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Precip = aggregate(PrecipCustom~season+Year+GCM+CF,Baseline_all,sum,na.rm=TRUE)
Precip = aggregate(PrecipCustom~season+GCM+CF,Precip,mean,na.rm=TRUE)
Baseline_all$RHmean<-(Baseline_all$RHmaxCustom+Baseline_all$RHminCustom)/2
RHmean = aggregate(RHmean~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)

H_Season<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean))
rm(Tmax,Tmin,Tmean,Precip,RHmean)

# Future
Tmax = aggregate(TmaxCustom~season+GCM+CF,Future_all,mean,na.rm=TRUE)
Tmin = aggregate(TminCustom~season+GCM+CF,Future_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgCustom~season+GCM+CF,Future_all,mean,na.rm=TRUE)
Precip = aggregate(PrecipCustom~season+Year+GCM+CF,Future_all,sum,na.rm=TRUE)
Precip = aggregate(PrecipCustom~season+GCM+CF,Precip,mean,na.rm=TRUE)
Future_all$RHmean<-(Future_all$RHmaxCustom+Future_all$RHminCustom)/2
RHmean = aggregate(RHmean~season+GCM+CF,Future_all,mean,na.rm=TRUE)

F_Season<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean))
rm(Tmax,Tmin,Tmean,Precip,RHmean)


################################ SUMMARIZE TEMPERATURE AND PRECIP BY MONTH & SEASON #######################

# Monthly abs
H_MonMean<-aggregate(cbind(TmaxCustom,TminCustom,TavgCustom,PrecipCustom,RHmean)~Month,H_Monthly,mean)
H_MonMean$CF<-"Historical";H_MonMean<-H_MonMean[,c("Month","CF",names(H_MonMean[,2:6]))]
F_MonCF<-aggregate(cbind(TmaxCustom,TminCustom,TavgCustom,PrecipCustom,RHmean)~Month+CF,F_Monthly,mean)
Monthly<-rbind(H_MonMean,F_MonCF)
Monthly$CF<-factor(Monthly$CF,levels = c(CFs,"Historical"))

# Monthly delta
Monthly_delta<-F_MonCF
for (i in 3:7){
  Monthly_delta[,i]<-F_MonCF[,i]-H_MonMean[,i][match(F_MonCF$Month,H_MonMean$Month)]
}
Monthly_delta$CF<-factor(Monthly_delta$CF,levels = c(CFs))

# Seasonal abs
H_SeasMean<-aggregate(cbind(TmaxCustom,TminCustom,TavgCustom,PrecipCustom,RHmean)~season,H_Season,mean)
H_SeasMean$CF<-"Historical";H_SeasMean<-H_SeasMean[,c("season","CF",names(H_SeasMean[,2:6]))]
F_SeasCF<-aggregate(cbind(TmaxCustom,TminCustom,TavgCustom,PrecipCustom,RHmean)~season+CF,F_Season,mean)
Season<-rbind(H_SeasMean,F_SeasCF)
Season$CF<-factor(Season$CF,levels = c(CFs,"Historical"))
Season$season = factor(Season$season, levels = c("Winter","Spring","Summer","Fall"))

# Season delta
Season_delta<-F_SeasCF
for (i in 3:7){
  Season_delta[,i]<-F_SeasCF[,i]-H_SeasMean[,i][match(F_SeasCF$season,H_SeasMean$season)]
}; Season_delta$CF<-factor(Season_delta$CF,levels = c(CFs))
Season_delta$season = factor(Season_delta$season, levels = c("Winter","Spring","Summer","Fall"))

########################################## END MONTH & SEASON SUMMARY ##########################################


######################################## CALCULATE ANNUAL DAYS ABOVE/BELOW TEMP & PRECIP THRESHOLDS ##########################

###### TOTAL & CONSECUTIVE DAYS OVER/UNDER THRESHOLD TEMPS ######

HistYears = length(unique(Baseline_all$Date$year))

HistTmax99 = quantile(Baseline_all$TmaxCustom, 0.99)
HistTmaxHigh = quantile(Baseline_all$TmaxCustom, QuantileHigh)
HistTminLow = quantile(Baseline_all$TminCustom, QuantileLow)
HistPrecip95 = quantile(Baseline_all$PrecipCustom[which(Baseline_all$PrecipCustom > 0.05)], 0.95) #percentil of days receiving precip
HistPr99 = quantile(Baseline_all$PrecipCustom[which(Baseline_all$PrecipCustom > 0.05)], 0.99)



Baseline_all$Julian = Baseline_all$Date$yday
Baseline_all<-Baseline_all[with(Baseline_all,order(Year,GCM,Julian)),]
Baseline_all$TavgCustom = (Baseline_all$TmaxCustom + Baseline_all$TminCustom)/2
Baseline_all$OverHotTemp = Baseline_all$TmaxCustom > HotTemp
Baseline_all$OverHighQ = Baseline_all$TmaxCustom > HistTmaxHigh
Baseline_all$Tmax99 = Baseline_all$TmaxCustom > HistTmax99
Baseline_all$HeatConsecutive=(Baseline_all$Tmax99)*unlist(lapply(rle(Baseline_all$Tmax99)$lengths, seq_len))
Baseline_all$UnderColdTemp = Baseline_all$TminCustom < ColdTemp
Baseline_all$UnderLowQ = Baseline_all$TminCustom < HistTminLow
Baseline_all$HeatConsecutive=(Baseline_all$OverHotTemp)*unlist(lapply(rle(Baseline_all$OverHotTemp)$lengths, seq_len))
Baseline_all$ColdConsecutive=(Baseline_all$UnderColdTemp)*unlist(lapply(rle(Baseline_all$UnderColdTemp)$lengths, seq_len))
Baseline_all$NoPrecip = Baseline_all$PrecipCustom < PrecipThreshold
Baseline_all$NoPrecipLength = (Baseline_all$NoPrecip)*unlist(lapply(rle(Baseline_all$NoPrecip)$lengths, seq_len)) 
Baseline_all$OverPrecip95 = Baseline_all$PrecipCustom > HistPrecip95
Baseline_all$OverPrecip99 = Baseline_all$PrecipCustom > HistPr99
Baseline_all$PrecipOver1 = Baseline_all$PrecipCustom > 1
Baseline_all$PrecipOver2 = Baseline_all$PrecipCustom > 2
Baseline_all$FThaw = Baseline_all$TminCustom<28 & Baseline_all$TmaxCustom>34
Baseline_all$GDD = Baseline_all$TavgCustom>41 # 5 deg C
Baseline_all$GDD_count = Baseline_all$GDD * unlist(lapply(rle(Baseline_all$GDD)$lengths, seq_len))
Baseline_all$N_GDD_count = (Baseline_all$GDD == FALSE) * unlist(lapply(rle(Baseline_all$GDD)$lengths, seq_len))
Baseline_all$HI = heat_index(Baseline_all$TmaxCustom,Baseline_all$RHminCustom)
Baseline_all$HI.EC = Baseline_all$HI >89 & Baseline_all$HI <103
Baseline_all$HI.Dan = Baseline_all$HI >102 & Baseline_all$HI < 124
Baseline_all$Frost = Baseline_all$GDD == TRUE & Baseline_all$TminCustom < 32

Future_all$Julian = Future_all$Date$yday
Future_all<-Future_all[with(Future_all,order(Year,GCM,Julian)),]
Future_all$TavgCustom = (Future_all$TmaxCustom + Future_all$TminCustom)/2
Future_all$OverHotTemp = Future_all$TmaxCustom > HotTemp
Future_all$OverHighQ = Future_all$TmaxCustom > HistTmaxHigh
Future_all$Tmax99 = Future_all$TmaxCustom > HistTmax99
Future_all$HeatConsecutive=(Future_all$Tmax99)*unlist(lapply(rle(Future_all$Tmax99)$lengths, seq_len))
Future_all$UnderColdTemp = Future_all$TminCustom < ColdTemp
Future_all$UnderLowQ = Future_all$TminCustom < HistTminLow
Future_all$HeatConsecutive=(Future_all$OverHotTemp)*unlist(lapply(rle(Future_all$OverHotTemp)$lengths, seq_len))
Future_all$ColdConsecutive=(Future_all$UnderColdTemp)*unlist(lapply(rle(Future_all$UnderColdTemp)$lengths, seq_len))
Future_all$NoPrecip = Future_all$PrecipCustom < PrecipThreshold
Future_all$NoPrecipLength = (Future_all$NoPrecip)*unlist(lapply(rle(Future_all$NoPrecip)$lengths, seq_len)) 
Future_all$OverPrecip95 = Future_all$PrecipCustom > HistPrecip95
Future_all$OverPrecip99 = Future_all$PrecipCustom > HistPr99
Future_all$PrecipOver1 = Future_all$PrecipCustom > 1
Future_all$PrecipOver2 = Future_all$PrecipCustom > 2
Future_all$FThaw = Future_all$TminCustom<28 & Future_all$TmaxCustom>34
Future_all$GDD = Future_all$TavgCustom>41 # 5 deg C
Future_all$GDD_count = Future_all$GDD * unlist(lapply(rle(Future_all$GDD)$lengths, seq_len))
Future_all$N_GDD_count = (Future_all$GDD == FALSE) * unlist(lapply(rle(Future_all$GDD)$lengths, seq_len))
Future_all$HI = heat_index(Future_all$TmaxCustom,Future_all$RHminCustom)
Future_all$HI.EC = Future_all$HI >89 & Future_all$HI <103
Future_all$HI.Dan = Future_all$HI >102 & Future_all$HI < 124
Future_all$Frost = Future_all$GDD == TRUE & Future_all$TminCustom < 32

#### Historical Dataframes aggregated by Year+GCM ###########
H_annual<-aggregate(cbind(PrecipCustom,OverHotTemp, OverHighQ, Tmax99, UnderColdTemp,UnderLowQ,  
                          NoPrecip, NoPrecipLength, OverPrecip95, OverPrecip99, PrecipOver1, PrecipOver2,
                          FThaw, GDD,HI.EC,HI.Dan)~CF+GCM+Year,Baseline_all,sum)

Hmeans<-aggregate(cbind(TmaxCustom,TminCustom,TavgCustom,RHmean)~CF+GCM+Year,Baseline_all,FUN=mean)
H_annual<-merge(H_annual,Hmeans,by=c("CF","GCM","Year"));rm(Hmeans)

# Agrregate mean w/ temps only W months
H.WinterTemp<-aggregate(TavgCustom~CF+GCM+Year,data=subset(Baseline_all,Month<3 | Month>11), mean)
colnames(H.WinterTemp)[4]<-"W.Temp"
H_annual <- merge(H_annual,H.WinterTemp,by=c("CF","GCM","Year")); rm(H.WinterTemp)

# Further Growing Season Calculations
Historical_GS <- as.data.table(subset(Baseline_all,select=c(Year,CF,GCM,Julian,GDD_count,N_GDD_count)))
Historical_GS<-Historical_GS[with(Historical_GS, order(Julian,Year,GCM)), ]
Historical_GU<-Historical_GS[GDD_count==7,.SD[1],by=.(Year,CF,GCM)] 

Historical_SE<-Historical_GS[N_GDD_count==6,.SD[c(.N)],by=.(Year,CF,GCM)]
Historical_SE$adjusted<-Historical_SE$Julian - 6

H<-aggregate(cbind(Julian)~CF+GCM+Year,data=Historical_GU,mean,na.rm=TRUE)
colnames(H)[4] <- "BegGrow"
H<-merge(H,Historical_SE[,c("CF","GCM","Year","adjusted")], by=c("CF","Year","GCM"),all=T)
colnames(H)[5] <- "EndGrow"
H$GrowLen<- H$EndGrow - H$BegGrow
H_annual<-merge(H_annual,H,by=c("CF","GCM","Year"))
rm(Historical_GS,Historical_GU,Historical_SE,H)

# Frost length calculations - late spring freeze events
Sp.Frost<-aggregate(Frost~CF+GCM+Year,data=subset(Baseline_all,Julian<180),sum)
colnames(Sp.Frost)[4] <- "Sp.Frost"
H_annual<-merge(H_annual,Sp.Frost,by=c("CF","GCM","Year"));rm(Sp.Frost)


#### Future Dataframes aggregated by Year+GCM ###########
F_annual<-aggregate(cbind(PrecipCustom,OverHotTemp, OverHighQ, Tmax99, UnderColdTemp,UnderLowQ,  
                          NoPrecip, NoPrecipLength, OverPrecip95, OverPrecip99, PrecipOver1, PrecipOver2,
                          FThaw, GDD,HI.EC,HI.Dan)~CF+GCM+Year,Future_all,sum)

Fmeans<-aggregate(cbind(TmaxCustom,TminCustom,TavgCustom,RHmean)~CF+GCM+Year,Future_all,FUN=mean)
F_annual<-merge(F_annual,Fmeans,by=c("CF","GCM","Year"));rm(Fmeans)

# Agrregate mean w/ temps only W months
F.WinterTemp<-aggregate(TavgCustom~CF+GCM+Year,data=subset(Future_all,Month<3 | Month>11), mean)
colnames(F.WinterTemp)[4]<-"W.Temp"
F_annual <- merge(F_annual,F.WinterTemp,by=c("CF","GCM","Year")); rm(F.WinterTemp)

# Further Growing Season Calculations
Future_GS <- as.data.table(subset(Future_all,select=c(Year,CF,GCM,Julian,GDD_count,N_GDD_count)))
Future_GS<-Future_GS[with(Future_GS, order(Julian,Year,GCM)), ]
Future_GU<-Future_GS[GDD_count==7,.SD[1],by=.(Year,CF,GCM)] 

Future_SE<-Future_GS[N_GDD_count==6,.SD[c(.N)],by=.(Year,CF,GCM)]
Future_SE$adjusted<-Future_SE$Julian - 6

F<-aggregate(cbind(Julian)~CF+GCM+Year,data=Future_GU,mean,na.rm=TRUE)
colnames(F)[4] <- "BegGrow"
F<-merge(F,Future_SE[,c("CF","GCM","Year","adjusted")], by=c("CF","Year","GCM"))
colnames(F)[5] <- "EndGrow"
F$GrowLen<- F$EndGrow - F$BegGrow
F_annual<-merge(F_annual,F,by=c("CF","GCM","Year"))
rm(Future_GS,Future_GU,Future_SE,F)

# Frost length calculations - late spring freeze events
Sp.Frost<-aggregate(Frost~CF+GCM+Year,data=subset(Future_all,Julian<180),sum)
colnames(Sp.Frost)[4] <- "Sp.Frost"
F_annual<-merge(F_annual,Sp.Frost,by=c("CF","GCM","Year"));rm(Sp.Frost)


######################################## END THRESHOLD CALCULATIONS ##############################

#### SetWD and save
setwd(WD_plots)

##### Save Current workspace environment
save.image(sprintf("%s_%s_%s_Final_Environment.RData",SiteID, Lat, Lon))
