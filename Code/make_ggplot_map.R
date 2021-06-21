###########################################################
###   MAP USING GGPLOT2   #################################
###########################################################

# from https://r-spatial.github.io/stars/articles/stars1.html
# Could not make it work with tmap but worth trying again

rm(list = ls())

st <- read_stars("./data/met/ACCESS1-3_rcp45_BCSD_met_1950.nc4", sub = "tmax", curvilinear = c("longitude", "latitude"))

st1 <- st_set_dimensions(st, 3, values = as.character(st_get_dimension_values(st, 3))) # In st1, time now has a value

st_slice <- slice(st, index = 17, along = "time")

# This makes a nice plot

ggplot() + 
  geom_stars(data = st_slice, alpha = 0.8) + 
  #facet_wrap("time") + 
  scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))

st_slice

slice_wrst <- st_slice[wrst]

ggplot() + 
  geom_stars(data = slice_wrst, alpha = 0.8) + 
  #facet_wrap("time") + 
  scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))

test
