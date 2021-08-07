## Testing chunks

################################
#### TEST RDS W/ WEIRD EXTENSIONS
library(stars);library(dplyr);library(ggplot2);library(ggthemes);library(viridis);library(here);library(ggrepel);library(rlang);library(ggbiplot);library(units)

test <- readRDS("C:/Users/achildress/Documents/wrst_temp/Data/WRST_simple/inmcm4.rcp85/Annual.tmeanF_inmcm4_rcp85")


ggplot() + # Resolution is course
  geom_stars(data = hist1[7,], alpha = 0.8) + 
  # facet_wrap("time") +
  # scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))


###################################
#### Precip troubleshooting
# 1. compare met to vic --> vic is total monthly, met is mean daily for each month
# 2. why deltas are negative --> st_apply(sum) is sum of all values -- hist has 20 more years, so greater
  # solution: divide by # years in dataset; still need to be careful whether working with mean daily or mean monthly
  # Values for locations w/ high precip 1/2 that of nClimGrid dataset
  
met_hist <- readRDS("C:/Users/achildress/Documents/wrst_temp/Data/WRST_simple/inmcm4.rcp85/cropped_st_hist_inmcm4_rcp85")
vic_hist <- readRDS("C:/Users/achildress/Documents/wrst_temp/Data/WRST_simple/inmcm4.rcp85/cropped_st_hist_wf_inmcm4_rcp85")

met_fut <- readRDS("C:/Users/achildress/Documents/wrst_temp/Data/WRST_simple/inmcm4.rcp85/cropped_st_fut_inmcm4_rcp85")
vic_fut <- readRDS("C:/Users/achildress/Documents/wrst_temp/Data/WRST_simple/inmcm4.rcp85/cropped_st_fut_wf_inmcm4_rcp85")

historical.period <- as.character(seq(1950,1999,1))
future.period <- as.character(seq(2025,2055,1))
##### MET CALCULATIONS
hist_var_met <- list()

for(H in 1:length(met_hist)){
  s = met_hist[[H]]
  s = select(s, pcp)
  hist_var_met[[H]] = s[,,,] #set for months
}

fut_var_met <- list()

for(F in 1:length(met_fut)){
  s = met_fut[[F]]
  s = select(s, pcp)
  fut_var_met[[F]] = s[,,,] #set for months
}

hist_var_met_st <- Reduce(c, hist_var_met)
hist_var_met_st %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_var_met_st

fut_var_met_st <- Reduce(c, fut_var_met)
fut_var_met_st %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_met_st

sum_hist_met <- st_apply(hist_var_met_st, c("x", "y"), FUN = function(x) sum(x)/length(historical.period)) # find mean
sum_fut_met <- st_apply(fut_var_met_st, c("x", "y"), FUN = function(x) sum(x)/length(future.period))
delta_met <- sum_fut_met - sum_hist_met


##### VIC CALCULATIONS
hist_var_vic <- list()

for(H in 1:length(vic_hist)){
  s = vic_hist[[H]]
  s = select(s, PRCP)
  hist_var_vic[[H]] = s[,,,] #set for months
}

fut_var_vic <- list()

for(F in 1:length(vic_fut)){
  s = vic_fut[[F]]
  s = select(s, PRCP)
  fut_var_vic[[F]] = s[,,,] #set for months
}

hist_var_vic_st <- Reduce(c, hist_var_vic)
hist_var_vic_st$PRCP <- drop_units(hist_var_vic_st$PRCP)
hist_var_vic_st %>% mutate(pcp_in = PRCP / 25.4) %>% select(pcp_in) -> hist_var_vic_st

fut_var_vic_st <- Reduce(c, fut_var_vic)
fut_var_vic_st$PRCP <- drop_units(fut_var_vic_st$PRCP)
fut_var_vic_st %>% mutate(pcp_in = PRCP / 25.4) %>% select(pcp_in) -> fut_var_vic_st


sum_hist_vic <- st_apply(hist_var_vic_st, c("x", "y"), FUN = function(x) sum(x)/length(historical.period)) # find mean
sum_fut_vic <- st_apply(fut_var_vic_st, c("x", "y"), FUN = function(x) sum(x)/length(future.period))
delta_vic <- sum_fut_vic - sum_hist_vic


####################################################
######### Testing calculating days over threshold
# Days over 1 in -- seems to work (and can likely do multiple threshold calcs in same obj)
# But threshold exceedence is really low. Check with Jeremy to see if this is BCSD limitation


fut_var_met_st %>% mutate(Over1 = pcp_in >= 1) -> fut_met_test
sum_yr <- st_apply(fut_met_test, c("x", "y"), FUN = function(x) sum(x)/length(future.period))

fut_var_vic_st %>% mutate(Over1 = pcp_in >= 1) -> fut_vic_test
sum_yr <- st_apply(fut_vic_test, c("x", "y"), FUN = function(x) sum(x)/length(future.period))

ggplot() + # Resolution is course
  geom_stars(data = sum_hist_met, alpha = 0.8) + 
  # facet_wrap("time") +
  # scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))
