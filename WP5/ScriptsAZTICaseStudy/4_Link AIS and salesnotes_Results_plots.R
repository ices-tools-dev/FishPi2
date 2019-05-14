
# R 3.4.4 
###


rm(list=ls())
library(vmstools)
library(RColorBrewer)
library(sp)
library(raster)
library(ggplot2)
library(plyr)

# Set paths ----
########################################### #
path<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Data"
path.res<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Results\\_trip"


# load results
setwd(path)

load("LinkedTacsatEflalo.RData")


  
###################       SOME PLOTS         ############### ####
############################################################ #

setwd(path.res)
gearlist <- unique(tacsat$LE_GEAR[!is.na(tacsat$LE_GEAR)])

xlim <- c(-4,-1)
ylim <- c(43,44)

# ltl
# xlim <- c(-4.5,-1)
# ylim <- c(43,45.5)



## . a) split among trips ----

 for (gear in gearlist){
  temp <- as.data.frame(AllTacsatEflalo [[gear]] )
  temp <- subset(temp, SI_STATE=="f")
# Plot effort
require(RColorBrewer)
png(filename=paste(gear,"_effort_trip.png", sep=""), width=900, height = 900)
plotTools(temp,level="gridcell" ,xlim=xlim, ylim=ylim, zlim=NULL,log=F,gridcell=c(0.01,0.01),color=NULL,control.tacsat=list(clm="effishing"))
title(paste(gear,"_effort_trip", sep=""))
dev.off()
# kg por especie - trip
  for (i in c("LE_KG_COE" ,"LE_KG_HKE" ,"LE_KG_HOM", "LE_KG_MAC" ,"LE_KG_MUR", "LE_KG_ALB")){
      #windows(8,8)
      png(filename=paste(gear,i,"trip.png", sep="_"), width=900, height = 900)
      plotTools(temp,level="gridcell" ,xlim=xlim, ylim=ylim, log=F,gridcell=c(0.01,0.01),
                color=NULL,control.tacsat=list(clm= c(i)))
      title(paste(gear,i, sep="_"))
      dev.off()
  }

}
  
## . b) split among vessel+ month + gear ----

for (gear in gearlist){
  temp <- as.data.frame(AllTacsatEflalo_month [[gear]] )
  temp <- subset(temp, SI_STATE=="f")
  
  # Plot effort
  require(RColorBrewer)
  png(filename=paste(gear,"_effort_month.png", sep=""), width=900, height = 900)
  plotTools(temp,level="gridcell" ,xlim=xlim, ylim=ylim, zlim=NULL,log=F,gridcell=c(0.01,0.01),color=NULL,control.tacsat=list(clm="effishing"))
  title(paste(gear,"_effort_month", sep=""))
  dev.off()
  # kg por especie - trip
  for (i in c("LE_KG_COE" ,"LE_KG_HKE" ,"LE_KG_HOM", "LE_KG_MAC" ,"LE_KG_MUR", "LE_KG_ALB")){
    #windows(8,8)
    png(filename=paste(gear,i,"month.png", sep="_"), width=900, height = 900)
    plotTools(temp,level="gridcell" ,xlim=xlim, ylim=ylim, log=F,gridcell=c(0.01,0.01),
              color=NULL,control.tacsat=list(clm= c(i)))
    title(paste(gear,i, sep="_"))
    dev.off()
  }
}



## . c) split among vessel+ month + gear  -> conserve total catch ----

for (gear in gearlist){
  temp <- as.data.frame(AllTacsatEflalo_conserve [[gear]] )
  temp <- subset(temp, SI_STATE=="f")
  
  # Plot effort
  require(RColorBrewer)
  png(filename=paste(gear,"_effort_conserve.png", sep=""), width=900, height = 900)
  plotTools(temp,level="gridcell" ,xlim=xlim, ylim=ylim, zlim=NULL,log=F,gridcell=c(0.01,0.01),color=NULL,control.tacsat=list(clm="effishing"))
  title(paste(gear,"_effort_conserve", sep=""))
  dev.off()
  # kg por especie - trip
  for (i in c("LE_KG_COE" ,"LE_KG_HKE" ,"LE_KG_HOM", "LE_KG_MAC" ,"LE_KG_MUR", "LE_KG_ALB")){
    #windows(8,8)
    png(filename=paste(gear,i,"conserve.png", sep="_"), width=900, height = 900)
    plotTools(temp,level="gridcell" ,xlim=xlim, ylim=ylim, log=F,gridcell=c(0.01,0.01),
              color=NULL,control.tacsat=list(clm= c(i)))
    title(paste(gear,i, sep="_"))
    dev.off()
  }
}




# plot tracks for every gear ----
####################################

setwd(path.res)

for (gear in gearlist){
  
  temp <- as.data.frame(AllTacsatEflalo [[gear]] )
 
  ves_ylim <- c(min(temp$SI_LATI)-0.01, max(temp$SI_LATI)+0.01)
  ves_xlim <- c(min(temp$SI_LONG)-0.01, max(temp$SI_LONG)+0.01)
  cex.val=0.5
  
  #ves_ylim <- c(43.31972, 45.35429)
  #ves_xlim <- c(-4.26278, -1.774478)
  
  #windows()
  map1 <- ggplot() +
    ggspatial::geom_spatial(data = coastline_pol, fill = "lightgrey", colour = "black",  cex=cex.val) +
    geom_point(data = temp[temp$SI_STATE=="s",], mapping = aes(x = SI_LONG, y = SI_LATI), shape=21, col="black", cex=cex.val) +
    geom_point(data = temp[temp$SI_STATE=="f",], mapping = aes(x = SI_LONG, y = SI_LATI), shape=21, col="green", cex=cex.val) +
    geom_point(data = harbours, mapping = aes(x = lon, y = lat), shape=16, col="red", cex=cex.val+2) +
    #theme_void() +
    coord_map(xlim = ves_xlim,ylim = ves_ylim)+
    ggtitle(gear)+
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(map1, filename = paste(gear, ".png", sep=""), width = 10, height = 10)
}
