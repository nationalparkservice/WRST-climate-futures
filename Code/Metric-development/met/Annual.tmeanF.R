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

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

grid_var_stars %>% mutate()
mutate(year = year(time)) %>%
  group_by(year) %>%
  summarise(Annual.tmean = mean(tmean_f,na.rm=TRUE)) %>%
  as.data.frame() -> grid_df


grid_df <- as_tibble(grid_var_stars) %>% mutate(year = as.factor(year(time))) %>%
  group_by(year) #%>% summarise(Annual.tmean = mean(tmean_f,na.rm=TRUE))

mean_annual 