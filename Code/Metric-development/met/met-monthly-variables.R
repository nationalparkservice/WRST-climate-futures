monthly.variables <- c("Annual.tmeanF","Annual.precipIn","DJF.tmeanF", "MAM.tmeanF", "JJA.tmeanF", "SON.tmeanF","DJF.tmaxF", 
                       "MAM.tmaxF", "JJA.tmaxF", "SON.tmaxF","DJF.tminF", "MAM.tminF", "JJA.tminF", "SON.tminF", 
                       "DJF.precipIn", "MAM.precipIn", "JJA.precipIn","SON.precipIn")

DF.hist = CF_GCM[,1:2]; DF.hist$Period = "Historical"
DF.hist[,monthly.variables] <- NA
DF.fut = CF_GCM[,1:2]; DF.fut$Period = "Future"
DF.fut[,monthly.variables] <- NA
DF.grid = data.frame(CF = "Historical",GCM = "Daymet", Period = "Historical")
DF.grid[,monthly.variables] <- NA

# DAYMET ----
cropped_st_grid <- readRDS(paste(data.dir,"cropped_st_Daymet",sep="/"))

# DJF Tmean ----
var = "DJF.tmeanF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s
  s = select(s, tmean)
  grid_var[[F]] = s[,,,c(1:2,12)] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmean <- drop_units(grid_var_stars$tmean)
grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)

# assign(paste0("grid_",var), mean_grid)

# MAM Tmean ----
var = "MAM.tmeanF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s
  s = select(s, tmean)
  grid_var[[F]] = s[,,,3:5] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmean <- drop_units(grid_var_stars$tmean)
grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# JJA Tmean ----
var = "JJA.tmeanF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s
  s = select(s, tmean)
  grid_var[[F]] = s[,,,6:8] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmean <- drop_units(grid_var_stars$tmean)
grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# SON Tmean ----
var = "SON.TmeanF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s
  s = select(s, tmean)
  grid_var[[F]] = s[,,,9:11] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmean <- drop_units(grid_var_stars$tmean)
grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# DJF Tmax ----
var = "DJF.tmaxF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, tmax)
  grid_var[[F]] = s[,,,c(1:2,12)] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmax <- drop_units(grid_var_stars$tmax)
grid_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# MAM Tmax ----
var = "MAM.tmaxF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, tmax)
  grid_var[[F]] = s[,,,3:5] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmax <- drop_units(grid_var_stars$tmax)
grid_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# JJA Tmax ----
var = "JJA.tmaxF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, tmax)
  grid_var[[F]] = s[,,,6:8] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmax <- drop_units(grid_var_stars$tmax)
grid_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# SON Tmax ----
var = "SON.TmaxF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, tmax)
  grid_var[[F]] = s[,,,9:11] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmax <- drop_units(grid_var_stars$tmax)
grid_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# DJF Tmin ----
var = "DJF.tminF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, tmin)
  grid_var[[F]] = s[,,,c(1:2,12)] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmin <- drop_units(grid_var_stars$tmin)
grid_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# MAM Tmin ----
var = "MAM.tminF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, tmin)
  grid_var[[F]] = s[,,,3:5] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmin <- drop_units(grid_var_stars$tmin)
grid_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# JJA Tmin ----
var = "JJA.tminF"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, tmin)
  grid_var[[F]] = s[,,,6:8] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmin <- drop_units(grid_var_stars$tmin)
grid_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# SON Tmin ----
var = "SON.TminF"
hist_var <- list()

grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, tmin)
  grid_var[[F]] = s[,,,9:11] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmin <- drop_units(grid_var_stars$tmin)
grid_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)

DF.grid[var] = mean(mean_grid$mean, na.rm=TRUE)
# assign(paste0("grid_",var), mean_grid)

# DJF Precip ----
var = "DJF.precipIn"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, pcp)
  grid_var[[F]] = s[,,,c(1:2,12)] #set for months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$pcp <- drop_units(grid_var_stars$pcp)
grid_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> grid_var_stars

sum_grid <- st_apply(grid_var_stars, c("x", "y"), sum)

DF.grid[var] = mean(sum_grid$sum, na.rm=TRUE)
# assign(paste0("grid_",var), sum_grid)

# MAM Precip ----
var = "MAM.precipIn"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, pcp)
  grid_var[[F]] = s[,,,3:5] #set for months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$pcp <- drop_units(grid_var_stars$pcp)
grid_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> grid_var_stars

sum_grid <- st_apply(grid_var_stars, c("x", "y"), sum)

DF.grid[var] = mean(sum_grid$sum, na.rm=TRUE)
# assign(paste0("grid_",var), sum_grid)

# JJA precip ----
var = "JJA.precipIn"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, pcp)
  grid_var[[F]] = s[,,,6:8] #set for months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$pcp <- drop_units(grid_var_stars$pcp)
grid_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> grid_var_stars

sum_grid <- st_apply(grid_var_stars, c("x", "y"), sum)

DF.grid[var] = mean(sum_grid$sum, na.rm=TRUE)
# assign(paste0("grid_",var), sum_grid)

# SON Precip ----
var = "SON.precipIn"
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = select(s, pcp)
  grid_var[[F]] = s[,,,9:11] #set for months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$pcp <- drop_units(grid_var_stars$pcp)
grid_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> grid_var_stars

sum_grid <- st_apply(grid_var_stars, c("x", "y"), sum)

DF.grid[var] = mean(sum_grid$sum, na.rm=TRUE)
# assign(paste0("grid_",var), sum_grid)

for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  
  GR <- paste(gcm,rcp,sep=".")
  index <- match(GR, GCMs)
  print(c(index, GR))

  # stars objs
  cropped_st_hist <- readRDS(paste(data.dir,paste0("cropped_st_hist_",gcm,"_",rcp),sep="/"))
  cropped_st_fut <- readRDS(paste(data.dir,paste0("cropped_st_fut_",gcm,"_",rcp),sep="/"))

    # DJF Tmean ----
    var = "DJF.tmeanF"
  
  hist_var <- list()
  
  for(H in 1:length(cropped_st_hist)){
    s = cropped_st_hist[[H]]
    s %>% mutate(tmean = (tmax + tmin)/2) -> s 
    s = select(s, tmean)
    hist_var[[H]] = s[,,,c(1:2,12)] #all months
  }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      fut_var[[F]] = s[,,,c(1:2,12)] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> fut_var_stars

    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
   DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
   DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)

    # MAM Tmean ----
    var = "MAM.tmeanF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      hist_var[[H]] = s[,,,3:5] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      fut_var[[F]] = s[,,,3:5] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # JJA Tmean ----
    var = "JJA.tmeanF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      hist_var[[H]] = s[,,,6:8] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      fut_var[[F]] = s[,,,6:8] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist    
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # SON Tmean ----
    var = "SON.TmeanF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      hist_var[[H]] = s[,,,9:11] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      fut_var[[F]] = s[,,,9:11] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # DJF Tmax ----
    var = "DJF.tmaxF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, tmax)
      hist_var[[H]] = s[,,,c(1:2,12)] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, tmax)
      fut_var[[F]] = s[,,,c(1:2,12)] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # MAM Tmax ----
    var = "MAM.tmaxF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, tmax)
      hist_var[[H]] = s[,,,3:5] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, tmax)
      fut_var[[F]] = s[,,,3:5] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # JJA Tmax ----
    var = "JJA.tmaxF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, tmax)
      hist_var[[H]] = s[,,,6:8] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, tmax)
      fut_var[[F]] = s[,,,6:8] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist    
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # SON Tmax ----
    var = "SON.TmaxF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, tmax)
      hist_var[[H]] = s[,,,9:11] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, tmax)
      fut_var[[F]] = s[,,,9:11] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmax_f = tmax * 9/5 + 32) %>% select(tmax_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # DJF Tmin ----
    var = "DJF.tminF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, tmin)
      hist_var[[H]] = s[,,,c(1:2,12)] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, tmin)
      fut_var[[F]] = s[,,,c(1:2,12)] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # MAM Tmin ----
    var = "MAM.tminF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, tmin)
      hist_var[[H]] = s[,,,3:5] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, tmin)
      fut_var[[F]] = s[,,,3:5] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # JJA Tmin ----
    var = "JJA.tminF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, tmin)
      hist_var[[H]] = s[,,,6:8] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, tmin)
      fut_var[[F]] = s[,,,6:8] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist    
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # SON Tmin ----
    var = "SON.TminF"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, tmin)
      hist_var[[H]] = s[,,,9:11] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, tmin)
      fut_var[[F]] = s[,,,9:11] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmin_f = tmin * 9/5 + 32) %>% select(tmin_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    delta <- mean_fut - mean_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(mean_hist$mean, na.rm=TRUE)
    DF.fut[index,var] = mean(mean_fut$mean, na.rm=TRUE)
    
    # DJF Precip ----
    var = "DJF.precipIn"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, pcp)
      hist_var[[H]] = s[,,,c(1:2,12)] #set for months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, pcp)
      fut_var[[F]] = s[,,,c(1:2,12)] #set for months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_stars
    
    sum_hist <- st_apply(hist_var_stars, c("x", "y"), FUN=function(x) sum(x)*90/length(historical.period)) # find sum
    sum_fut <- st_apply(fut_var_stars, c("x", "y"), FUN=function(x) sum(x)*90/length(future.period))
    delta <- sum_fut - sum_hist
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    DF.hist[index,var] = mean(sum_hist$pcp_in, na.rm=TRUE)
    DF.fut[index,var] = mean(sum_fut$pcp_in, na.rm=TRUE)
    
    # MAM Precip ----
  var = "MAM.precipIn"
  hist_var <- list()
  
  for(H in 1:length(cropped_st_hist)){
    s = cropped_st_hist[[H]]
    s = select(s, pcp)
    hist_var[[H]] = s[,,,3:5] #set for months
  }
  
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s = select(s, pcp)
    fut_var[[F]] = s[,,,3:5] #set for months
  }
  
  hist_var_stars <- Reduce(c, hist_var)
  hist_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_var_stars
  
  fut_var_stars <- Reduce(c, fut_var) 
  fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_stars
  
  sum_hist <- st_apply(hist_var_stars, c("x", "y"), FUN=function(x) sum(x)*90/length(historical.period)) # find sum
  sum_fut <- st_apply(fut_var_stars, c("x", "y"), FUN=function(x) sum(x)*90/length(future.period))
  delta <- sum_fut - sum_hist
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
  #### Add values to Means dfs
  DF.hist[index,var] = mean(sum_hist$pcp_in, na.rm=TRUE)
  DF.fut[index,var] = mean(sum_fut$pcp_in, na.rm=TRUE)
  
    # JJA precip ----
  var = "JJA.precipIn"
  hist_var <- list()
  
  for(H in 1:length(cropped_st_hist)){
    s = cropped_st_hist[[H]]
    s = select(s, pcp)
    hist_var[[H]] = s[,,,6:8] #set for months
  }
  
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s = select(s, pcp)
    fut_var[[F]] = s[,,,6:8] #set for months
  }
  
  hist_var_stars <- Reduce(c, hist_var)
  hist_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_var_stars
  
  fut_var_stars <- Reduce(c, fut_var) 
  fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_stars
  
  sum_hist <- st_apply(hist_var_stars, c("x", "y"), FUN=function(x) sum(x)*90/length(historical.period)) # find sum
  sum_fut <- st_apply(fut_var_stars, c("x", "y"), FUN=function(x) sum(x)*90/length(future.period))
  delta <- sum_fut - sum_hist
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
  #### Add values to Means dfs
  DF.hist[index,var] = mean(sum_hist$pcp_in, na.rm=TRUE)
  DF.fut[index,var] = mean(sum_fut$pcp_in, na.rm=TRUE)
  
    # SON Precip ----
  var = "SON.precipIn"
  hist_var <- list()
  
  for(H in 1:length(cropped_st_hist)){
    s = cropped_st_hist[[H]]
    s = select(s, pcp)
    hist_var[[H]] = s[,,,9:11] #set for months
  }
  
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s = select(s, pcp)
    fut_var[[F]] = s[,,,9:11] #set for months
  }
  
  hist_var_stars <- Reduce(c, hist_var)
  hist_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_var_stars
  
  fut_var_stars <- Reduce(c, fut_var) 
  fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_stars
  
  sum_hist <- st_apply(hist_var_stars, c("x", "y"), FUN=function(x) sum(x)*90/length(historical.period)) # find sum
  sum_fut <- st_apply(fut_var_stars, c("x", "y"), FUN=function(x) sum(x)*90/length(future.period))
  delta <- sum_fut - sum_hist
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
  #### Add values to Means dfs
  DF.hist[index,var] = mean(sum_hist$pcp_in, na.rm=TRUE)
  DF.fut[index,var] = mean(sum_fut$pcp_in, na.rm=TRUE)
}
DF = rbind(DF.hist, DF.fut,DF.grid)
write.csv(DF, paste0(data.dir,"/Monthly_met.csv"),row.names=FALSE)

rm(hist_var, fut_var, grid_var, hist_var_stars, fut_var_stars, grid_var_stars, sum_hist, sum_fut, sum_grid, 
   delta,mean_hist,mean_fut,mean_grid, baseline,future, cropped_st_fut, cropped_st_hist,cropped_st_grid)
gc()