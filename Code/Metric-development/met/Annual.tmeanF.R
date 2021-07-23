# Annual.tmean
var = "Annual.tmeanF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s 
  s = select(s, tmean)
  grid_var[[F]] = s[,,,] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmean <- drop_units(grid_var_stars$tmean)
grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> grid_var_stars


# st_get_dimension_values(grid_var_stars,"time") #how get time dimension values

by_t = "1 year"
test <- aggregate(grid_var_stars, by = by_t, FUN = mean, na.omit = TRUE) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
test2 <- split(test, "time")

df<-data.frame(year=daymet.period,mean=NA)
for (i in 1:length(daymet.period)){
t <-st_apply(test2[i],1:2,mean)
df$mean[i] <- mean(t$mean,na.rm=TRUE)
}

# test2 <- split(test, "time") # ggplot will not work if time is a dimension, so switching to an attribute. Should not matter since time is aggregated here. 
mean_grid <- st_apply(test, c("x", "y"), mean)
saveRDS(mean_grid, file = paste0(model.dir,"/",var,gcm))

# Plot

ggplot() + # Resolution is course
  geom_stars(data = mean_grid, alpha = 0.8) + 
  # facet_wrap("time") +
  # scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))


