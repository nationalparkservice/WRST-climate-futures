# runoff$name="runoff"; runoff$X=NULL;names(runoff)[2]="var"
# SWE$name="SWE"; SWE$X=NULL;names(SWE)[2]="var"

df = merge(SWE,runoff,by=c("time","GCM"))

df = merge(df, CF_GCM,by="GCM",all=TRUE)
df$CF[which(is.na((df$CF)))] = "Historical"
df$CF_col[which(is.na((df$CF_col)))] = "grey"
df$CF = factor(df$CF, levels=c(CFs,"Historical"))
df$date = as.Date(df$time, format="%Y-%m-%d")
df$Year = as.factor(substr(df$time, 1, 4))
df$yday = yday(df$date)

runoff.plot <- function(data, col,CF){  
ggplot() +
  geom_line(data=data,aes(x=yday,y=RUNOFF_in,group=Year),colour=col,size=.7) +
  theme(axis.text=element_text(size=16),
        # axis.text.x=element_blank(),
        axis.title.x=element_text(size=16,vjust=1.0),
        axis.title.y=element_text(size=16,vjust=1.0),
        plot.title=element_blank(),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.position = "bottom") +
  labs(title = "", 
       x = "", y = CF) +
  ylim(0,max(df$RUNOFF_in))
  # coord_fixed(ratio = .5)
}

CF1.runoff = runoff.plot(data=subset(df, CF == CFs[1]),col=cols[1],CF=CFs[1])
CF2.runoff = runoff.plot(data=subset(df, CF == CFs[2]),col=cols[2],CF=CFs[2])
CF3.runoff = runoff.plot(data=subset(df, CF == CFs[3]),col=cols[3],CF=CFs[3])
Hist.runoff = runoff.plot(data=subset(df, CF == "Historical"),col="grey",CF="Historical")


############################################################################
SWE.plot <- function(data, col,CF){  
  ggplot() +
    geom_line(data=data,aes(x=yday,y=SWE_in,group=Year),colour=col,size=.7) +
    theme(axis.text=element_text(size=16),
          # axis.text.x=element_blank(),
          axis.title.x=element_text(size=16,vjust=1.0),
          axis.title.y=element_text(size=16,vjust=1.0),
          plot.title=element_blank(),
          legend.text=element_text(size=14), legend.title=element_text(size=14),
          legend.position = "bottom") +
    labs(title = "", 
         x = "", y = "") +
    ylim(0,max(df$SWE_in))
  # coord_fixed(ratio = .5)
}

CF1.SWE = SWE.plot(data=subset(df, CF == CFs[1]),col=cols[1],CF=CFs[1])
CF2.SWE = SWE.plot(data=subset(df, CF == CFs[2]),col=cols[2],CF=CFs[2])
CF3.SWE = SWE.plot(data=subset(df, CF == CFs[3]),col=cols[3],CF=CFs[3])
Hist.SWE = SWE.plot(data=subset(df, CF == "Historical"),col="grey",CF="Historical")


#### Just maps and ts plot
grid1 <- ggarrange(Hist.runoff, CF1.runoff, CF2.runoff, CF3.runoff, ncol = 1, nrow = 4)

grid1 = annotate_figure(grid1, left = textGrob("Runoff (in)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                        bottom = textGrob("Julian day", gp = gpar(cex = 1.3)),
                        top = textGrob("Average daily runoff",
                                       gp=gpar(fontface="bold", col="black", fontsize=16)))

grid2 <- ggarrange(Hist.SWE, CF1.SWE, CF2.SWE, CF3.SWE, ncol = 1, nrow = 4)

grid2 =  annotate_figure(grid2, left = textGrob("SWE (in)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Julian day", gp = gpar(cex = 1.3)),
                top = textGrob("Average daily SWE",
                               gp=gpar(fontface="bold", col="black", fontsize=16)))

grid = ggarrange(grid1,grid2,nrow=1,ncol=2) 
annotate_figure(grid, top = textGrob(area,
                                      gp=gpar(fontface="bold", col="black", fontsize=20)))

ggsave(paste0(area,"_SWE-runoff.png"), width = 15, height = 9, path = plot.dir)
