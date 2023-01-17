rm(list = ls())

library(stars);library(dplyr);library(ggplot2);library(ggthemes);library(viridis);library(here);library(ggrepel)

OutDir <- "C:/Users/achildress/DOI/Climate Change Scenario Planning - WRST RSS/"

Deltas = read.csv("C:/Users/achildress/Documents/wrst_temp/T1-extraction/wrst_simple/Deltas-T1.csv")
Deltas$model = paste(Deltas$GCM,Deltas$RCP,sep=".")

GCMs <- c("inmcm4.rcp85","ACCESS1-3.rcp45","CanESM2.rcp85")
CFs <- c("Climate Future 1", "Climate Future 2", "Climate Future 3")
cols <- c("#12045C","#ffcccc","#E10720")

CF_GCM <- data.frame(CF=CFs,GCM=GCMs,CF_col=cols)

PlotWidth = 15
PlotHeight = 9

ggplot(data=Deltas, aes(x=Tmean_F, y=Precip_in*30))  +
    geom_text_repel(aes(label=model)) +
    geom_point(colour="black",size=4) +
  geom_point(aes(x=mean(Tmean_F[which(model==GCMs[1])]), y=mean(Precip_in[which(model==GCMs[1])]*30)), shape=21, size=14, stroke=3.5, colour=cols[1]) +
  geom_point(aes(x=mean(Tmean_F[which(model==GCMs[2])]), y=mean(Precip_in[which(model==GCMs[2])]*30)), shape=21, size=14, stroke=3.5, colour=cols[2]) +
  geom_point(aes(x=mean(Tmean_F[which(model==GCMs[3])]), y=mean(Precip_in[which(model==GCMs[3])]*30)), shape=21, size=14, stroke=3.5, colour=cols[3]) +
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

ggsave("Scatterplot-TvsP.png", width = 15, height = 9, path = OutDir)

#PCA plot
# PCA df setup
head(Deltas)
Deltas.rownames <- Deltas[,-c(1:2,16)]
rownames(Deltas.rownames) <- paste(Deltas$GCM,Deltas$RCP,sep=".")

PCA <- prcomp(Deltas.rownames[,c(1:length(Deltas.rownames))], center = TRUE,scale. = TRUE)

head(PCA$rotation)
head(PCA$x)
summary(PCA)

str(PCA)

biplot(PCA)

ggplot_pca(PCA)


ggbiplot(PCA, labels=rownames(Deltas.rownames))
ggsave("PCA-plot.png", path = plot.dir, width = PlotWidth, height = PlotHeight)

pca.x<-as.data.frame(PCA$x)

# This method doesn't return the exact models we selected,
# but we may not have selected the most divergent models
CF1 <- rownames(pca.x)[which.min(pca.x$PC1)]
CF2 <- rownames(pca.x)[which.max(pca.x$PC1)]
CF3 <- rownames(pca.x)[which.min(pca.x$PC2)]
CF4 <- rownames(pca.x)[which.max(pca.x$PC2)]
