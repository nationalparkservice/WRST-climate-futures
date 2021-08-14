long.names = c("Freeze thaw cycles", "Tmin < 32 (\u00B0F)", "Tmax > 68 (\u00B0F)",
               "Tmin > 32 (\u00B0F) in DJF", "Precipitation > 0.5 inches","Cumulative growing degrees (\u00B0F)")
ratios = c(1,1,1,1,1)

head(df)
df = merge(DF, CF_GCM,by="GCM",all=TRUE)
df$CF[which(is.na((df$CF)))] = "Historical"
df$CF_col[which(is.na((df$CF_col)))] = "grey"
df$CF = factor(df$CF, levels=c(CFs,"Historical"))
df$Year = as.Date(df$year, format="%Y-%m-%d")
df = subset(df, Year!="2017-08-13")
df$W.under32 = 90 - df$W.under32

means = df %>% group_by(CF) %>%
  summarize(mfreeze.thaw = mean(freeze.thaw), 
            munder32 = mean(under32),
            mover20 = mean(over20),
            mW.under32 = mean(W.under32),
            mpcp.over.5 = mean(pcp.over.5))

ts.plot <- function(data, var, title){
ggplot(data=data, aes(x=Year, y=eval(parse(text=var)), group=CF, colour = CF)) +
  
  geom_line(colour = "black",size=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        # axis.text.x=element_blank(),
        axis.title.x=element_text(size=16,vjust=1.0),
        axis.title.y=element_text(size=16,vjust=1.0),
        plot.title=element_blank(),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.position = "bottom") +
  labs(title = "", 
       x = "Year", y = title) +
  scale_color_manual(name="",values = c(cols,"grey")) +
  scale_fill_manual(name="",values = c(cols,"grey")) +
  scale_shape_manual(name="",values = c(21,22,23,24)) 
  # coord_fixed(ratio = .5)
}
freeze.thaw = ts.plot(data=df,var="freeze.thaw",title=long.names[1])
under32 = ts.plot(data=df,var="under32",title=long.names[2])
over20 = ts.plot(data=df,var="over20",title=long.names[3])
W.under32 = ts.plot(data=df,var="W.under32",title=long.names[4])
pcp.over.5 = ts.plot(data=df,var="pcp.over.5",title=long.names[5])
gdd = ts.plot(data=df,var="GDD",title=long.names[6])


#### Just maps and ts plot
grid1 <- grid_arrange_shared_legend(freeze.thaw,under32,over20, ncol = 1, nrow = 3, position = "bottom", 
                                   top = textGrob(paste0("Annual threshold exceedances for",area, "(days/year)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=16)))
grid2 <- grid_arrange_shared_legend(W.under32,pcp.over.5, ncol = 1, nrow = 2, position = "bottom", 
                                    top = textGrob(paste0("Annual threshold exceedances for",area, "(days/year)"),
                                                   gp=gpar(fontface="bold", col="black", fontsize=16)))
# g <- ggarrange(maps,ts, nrow=2)
# g

#### Maps, ts, table
delta.var <- data.frame(means)
names(delta.var) = c("CF","freeze-thaw", "tmin<32", "tmax>68","DJF>32","prcp>0.5")
for (i in 1:3){
  delta.var[i,2:6] = delta.var[i,2:6] - delta.var[4,2:6]
}
delta.var[,2:6] <- signif(delta.var[,2:6], digits = 1)

table <- tableGrob(delta.var, rows = NULL) 

table <- gtable_add_grob(table, grobs = rectGrob(gp = gpar(fill=NA, lwd=2)), #library(gtable)
                     t=5,b=nrow(table),l=1,r=ncol(table))
table <- annotate_figure(table,
                bottom = text_grob("Historical = absolute value; CFs = change values", color = "black",
                                 face = "italic", size = 12))
tsplots <- grid.arrange(grid2, table, nrow=2, heights=c(3,1), clip = FALSE)


g <- ggarrange(grid1,tsplots, ncol=2)
g

ggsave(paste0(area,"_ts-stack.png"), width = 15, height = 9, path = plot.dir)




