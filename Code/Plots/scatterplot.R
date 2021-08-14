Deltas = read.csv("C:/Users/achildress/Documents/wrst_temp/T1-extraction/wrst_simple/Deltas-T1.csv")
Deltas$model = paste(Deltas$GCM,Deltas$RCP,sep=".")

PlotWidth = 15
PlotHeight = 9

ggplot(data=Deltas, aes(x=Tmean_F, y=Precip_in))  +
    geom_text_repel(aes(label=model)) +
    geom_point(colour="black",size=4) +
  geom_point(aes(x=mean(Tmean_F[which(model==GCMs[1])]), y=mean(Precip_in[which(model==GCMs[1])])), shape=21, size=14, stroke=3.5, colour=cols[1]) +
  geom_point(aes(x=mean(Tmean_F[which(model==GCMs[2])]), y=mean(Precip_in[which(model==GCMs[2])])), shape=21, size=14, stroke=3.5, colour=cols[2]) +
  geom_point(aes(x=mean(Tmean_F[which(model==GCMs[3])]), y=mean(Precip_in[which(model==GCMs[3])])), shape=21, size=14, stroke=3.5, colour=cols[3]) +
    theme(axis.text=element_text(size=18),
          axis.title.x=element_text(size=18,vjust=-0.2),
          axis.title.y=element_text(size=18,vjust=0.2),
          plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
          legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
    ###
    labs(title ="Changes in projection means in 2040 (1950-1999 vs 2025-2055)", 
         x = paste0("Changes in ","Avg Annual Temp (F)"), # Change
         y = paste0("Changes in ","Avg Annual Precip (in)")) + #change
    scale_color_manual(name="Scenarios", values=c("black")) +
    # scale_fill_manual(name="Scenarios",values = c("black")) + 
    theme(legend.position="none") 

ggsave("Scatterplot-TvsP.png", width = 15, height = 9, path = plot.dir)
