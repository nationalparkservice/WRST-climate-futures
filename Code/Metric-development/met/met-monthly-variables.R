for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  # cf = CF_GCM$CF[match(gcm, CF_GCM$GCM)]
  model.dir <- paste0(data.dir,"/",GCMs[G])
  # stars objs
  cropped_st_hist <- readRDS(paste(model.dir,paste0("cropped_st_hist_",gcm,"_",rcp),sep="/"))
  cropped_st_fut <- readRDS(paste(model.dir,paste0("cropped_st_fut_",gcm,"_",rcp),sep="/"))
  # assign(cropped_st_hist,paste0("cropped_st_hist_",GCMs[G]))
  # assign(cropped_st_fut,paste0("cropped_st_fut_",GCMs[G]))

    # # Annual Tmean ----
    # var = "Annual.tmeanF"
    # hist_var <- list()
    # 
    # for(H in 1:length(cropped_st_hist)){
    #   s = cropped_st_hist[[H]]
    #   s %>% mutate(tmean = (tmax + tmin)/2) -> s 
    #   s = select(s, tmean)
    #   hist_var[[H]] = s[,,,] #all months
    # }
    # 
    # fut_var <- list()
    # 
    # for(F in 1:length(cropped_st_fut)){
    #   s = cropped_st_fut[[F]]
    #   s %>% mutate(tmean = (tmax + tmin)/2) -> s 
    #   s = select(s, tmean)
    #   fut_var[[F]] = s[,,,] #all months
    # }
    # 
    # hist_var_stars <- Reduce(c, hist_var)
    # hist_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> hist_var_stars
    # 
    # fut_var_stars <- Reduce(c, fut_var) 
    # fut_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> fut_var_stars
    # 
    # mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    # mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    # delta <- mean_fut - mean_hist
    # saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    # 
    # #### Add values to Means dfs
    # baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    # Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    # 
    # future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    # Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    # 
    # # Annual Precip ----
    # var = "Annual.precipIn"
    # hist_var <- list()
    # 
    # for(H in 1:length(cropped_st_hist)){
    #   s = cropped_st_hist[[H]]
    #   s = select(s, pcp)
    #   hist_var[[H]] = s[,,,] #set for months
    # }
    # 
    # fut_var <- list()
    # 
    # for(F in 1:length(cropped_st_fut)){
    #   s = cropped_st_fut[[F]]
    #   s = select(s, pcp)
    #   fut_var[[F]] = s[,,,] #set for months
    # }
    # 
    # hist_var_stars <- Reduce(c, hist_var)
    # hist_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_var_stars
    # 
    # fut_var_stars <- Reduce(c, fut_var) 
    # fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_stars
    # 
    # sum_hist <- st_apply(hist_var_stars, c("x", "y"), sum) # find sum
    # sum_fut <- st_apply(fut_var_stars, c("x", "y"), sum)
    # delta <- sum_fut - sum_hist
    # saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    # 
    # #### Add values to Means dfs
    # baseline <- data.frame(GCM=GCMs[G], var = mean(sum_hist$sum, na.rm=TRUE));  names(baseline)[2] <- var
    # Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    # 
    # future <- data.frame(GCM=GCMs[G], var = mean(sum_fut$sum, na.rm=TRUE));  names(future)[2] <- var
    # Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)

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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(mean_hist$mean, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(mean_fut$mean, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
    
    sum_hist <- st_apply(hist_var_stars, c("x", "y"), sum) # find sum
    sum_fut <- st_apply(fut_var_stars, c("x", "y"), sum)
    delta <- sum_fut - sum_hist
    saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
    
    #### Add values to Means dfs
    baseline <- data.frame(GCM=GCMs[G], var = mean(sum_hist$sum, na.rm=TRUE));  names(baseline)[2] <- var
    Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
    
    future <- data.frame(GCM=GCMs[G], var = mean(sum_fut$sum, na.rm=TRUE));  names(future)[2] <- var
    Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
    
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
  
  sum_hist <- st_apply(hist_var_stars, c("x", "y"), sum) # find sum
  sum_fut <- st_apply(fut_var_stars, c("x", "y"), sum)
  delta <- sum_fut - sum_hist
  saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
  #### Add values to Means dfs
  baseline <- data.frame(GCM=GCMs[G], var = mean(sum_hist$sum, na.rm=TRUE));  names(baseline)[2] <- var
  Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
  
  future <- data.frame(GCM=GCMs[G], var = mean(sum_fut$sum, na.rm=TRUE));  names(future)[2] <- var
  Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
  
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
  
  sum_hist <- st_apply(hist_var_stars, c("x", "y"), sum) # find sum
  sum_fut <- st_apply(fut_var_stars, c("x", "y"), sum)
  delta <- sum_fut - sum_hist
  saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
  #### Add values to Means dfs
  baseline <- data.frame(GCM=GCMs[G], var = mean(sum_hist$sum, na.rm=TRUE));  names(baseline)[2] <- var
  Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
  
  future <- data.frame(GCM=GCMs[G], var = mean(sum_fut$sum, na.rm=TRUE));  names(future)[2] <- var
  Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
  
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
  
  sum_hist <- st_apply(hist_var_stars, c("x", "y"), sum) # find sum
  sum_fut <- st_apply(fut_var_stars, c("x", "y"), sum)
  delta <- sum_fut - sum_hist
  saveRDS(delta, file = paste(model.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
  #### Add values to Means dfs
  baseline <- data.frame(GCM=GCMs[G], var = mean(sum_hist$sum, na.rm=TRUE));  names(baseline)[2] <- var
  Baseline_Monthly = merge(Baseline_Monthly,baseline,by="GCM",all=TRUE)
  
  future <- data.frame(GCM=GCMs[G], var = mean(sum_fut$sum, na.rm=TRUE));  names(future)[2] <- var
  Future_Monthly = merge(Future_Monthly,future,by="GCM",all=TRUE)
}

model.dir <- paste0(data.dir,"/", "Daymet")
cropped_st_grid <- readRDS(paste(model.dir,"cropped_st_Daymet",sep="/"))

# # Annual Tmean ----
# var = "Annual.tmeanF"
# grid_var <- list()
# 
# for(F in 1:length(cropped_st_grid)){
#   s = cropped_st_grid[[F]]
#   s %>% mutate(tmean = (tmax + tmin)/2) -> s 
#   s = select(s, tmean)
#   grid_var[[F]] = s[,,,] #all months
# }
# 
# grid_var_stars <- Reduce(c, grid_var)
# grid_var_stars$tmean <- drop_units(grid_var_stars$tmean)
# grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> grid_var_stars
# 
# mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)
# 
# grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
# Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)
# 
#   # Annual Precip ----
# var = "Annual.precipIn"
# grid_var <- list()
# 
# for(F in 1:length(cropped_st_grid)){
#   s = cropped_st_grid[[F]]
#   s = select(s, pcp)
#   grid_var[[F]] = s[,,,] #set for months
# }
# 
# grid_var_stars <- Reduce(c, grid_var)
# grid_var_stars$pcp <- drop_units(grid_var_stars$pcp)
# grid_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> grid_var_stars
# 
# sum_grid <- st_apply(grid_var_stars, c("x", "y"), sum)
# 
# grid <- data.frame(GCM="Daymet", var = mean(sum_grid$sum, na.rm=TRUE));  names(grid)[2] <- var
# Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(mean_grid$mean, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(sum_grid$sum, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(sum_grid$sum, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(sum_grid$sum, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)

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

grid <- data.frame(GCM="Daymet", var = mean(sum_grid$sum, na.rm=TRUE));  names(grid)[2] <- var
Daymet_Monthly = merge(Daymet_Monthly,grid,by="GCM",all=TRUE)


rm(hist_var, fut_var, grid_var, hist_var_stars, fut_var_stars, grid_var_stars, sum_hist, sum_fut, sum_grid, 
   delta,mean_hist,mean_fut,mean_grid, baseline,future, cropped_st_fut, cropped_st_hist,cropped_st_grid)
  