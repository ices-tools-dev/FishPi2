
# -- Load libraries ----
####################### #

rm(list=ls())
  library(vmstools)
  library(plyr)
  library (lubridate)
  library(rgdal)
  library(raster)
  library(rgeos)
  library(ggplot2)
  library("gridExtra")
  library(scales)

##### --  set directories and load data  ----
############################################### # 

path<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Data"
res.path<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Results\\_ais"
map.path<- "C:\\use\\0_Lucia\\10_Recursos\\mapas\\basemaps\\Europe_coastline_shapefile"

setwd(path)
load("Datos_art_2017_trip.RData")


######### --      subset      -- ########## #
########################################### # 

# subset eflalo to take only the vessel whe have AIS data for
tacsat_0 <- tacsat_art



######### --  explore data    ----
########################################### # 

sort(unique(tacsat_0$VE_REF))
  
str(tacsat_0)

head(tacsat_0)

summary(tacsat_0)


############## - cleaning spatial data  ----
######################################################### #



# 1.  Wrong VMS positions ####
#################################################### #

# VMS positions that are not actually on planet Earth
idx <- which(abs(tacsat_0$SI_LATI) > 90 | abs(tacsat_0$SI_LONG) > 180) #points not on the globe
idx <- unique(c(idx,which(tacsat_0$SI_HE < 0 | tacsat_0$SI_HE >  360))) #adding points with heading outside compass range
length(idx)


# VMS positions that are out of the spatial range of our artisanal fleet
windows()


tacsat_0 <- tacsat_0[tacsat_0$SI_LATI<46,]
tacsat_0 <- tacsat_0[tacsat_0$SI_LATI>42,]
tacsat_0 <- tacsat_0[tacsat_0$SI_LONG>(-5),]


# 2. Duplicates ####
#################################################### #

tacsat_0 <- sortTacsat(tacsat_0)  
tacsat_0$YEAR <- year(tacsat_0$SI_DATIM)

# we define duplicated with the combination of vessel name + lat + long + DATIM
uniqueTacsat    <- paste(tacsat_0$VE_REF,tacsat_0$SI_LATI,tacsat_0$SI_LONG,tacsat_0$SI_DATIM)

print(nrow(tacsat_0))  # n rows total
print(nrow(tacsat_0[duplicated(uniqueTacsat),]))  # n rows duplicated
head(tacsat_0[duplicated(uniqueTacsat),])  # duplicated rows, one trip selling in two ports

# get rid of the duplicates
tacsat_0  <- tacsat_0[!duplicated(uniqueTacsat),] 
print(nrow(tacsat_0))


# 
#3.  Points in land ####
#################################################### #

setwd(map.path)

## Read highresolution base map. 
country.poly <- readOGR('Europe_costline_poly_wgs84_EEA.shp')
extent(country.poly)

## Create the clipping polygon for our fleet
CP <- as(extent( -5, 0,42, 46), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(country.poly))
summary(CP)

## Clip the map
coastline_pol <- gIntersection(country.poly, CP, byid=TRUE)

## Convert to Spatial Polygon
country.poly <- SpatialPolygons(coastline_pol@polygons,proj4string=coastline_pol@proj4string)
summary(country.poly)


#Identify points on land
idx     <- pointOnLand(tacsat_0,country.poly,proj4string=coastline_pol@proj4string)
table (idx)
tacsat_land <- tacsat_0[which(idx==1),] #points on land
tacsat_sea <- tacsat_0[which(idx==0),] #points not on sea


#4. points in a harbour ####
#################################################### #

# We use our own data becasue the position of the ports is more accurate

setwd(path)
PortPosition <-read.csv("port position.csv", sep=",",header=T, stringsAsFactors = FALSE)#Con nuestros puertos solamente
str(PortPosition)
head(PortPosition)
PortPosition<- PortPosition[,c("LANDING_PORT_LABEL" ,"LANDING_PORT_LATITUDE","LANDING_PORT_LONGITUDE")]
PortPosition$lat<- as.numeric(PortPosition$LANDING_PORT_LATITUDE)
PortPosition$lon<- as.numeric(PortPosition$LANDING_PORT_LONGITUDE)
PortPosition$harbour<- as.factor(PortPosition$LANDING_PORT_LABEL)
names(PortPosition) <- c("harbour", "lat", "lon" )
PortPosition$range <- 0.5  # Distance in km that will be deleted around each port

harbours <- PortPosition[,c("harbour", "lat", "lon", "range")]

#points in harbour
idx <- pointInHarbour(lon=tacsat_sea$SI_LONG,lat=tacsat_sea$SI_LATI,harbours=harbours,saveHarbourList=F)
table(idx)
tacsat_har <- tacsat_sea[which(idx==1),] #points on harbour
tacsat <- tacsat_sea[which(idx==0),] #points not on harbour



#3. VMS records associated with very high speeds ####
#################################################### #


#We set in 20knots is the maximum speed allowed in the dataset
windows()
hist(tacsat$SI_SP,breaks=10)
spThres <- 20 
idx <- which(tacsat$SI_SP > spThres)
tacsat <- tacsat[-idx,]


#4. Anomalous intervals ####
#################################################### #


# define trip (one-day trips. we sum one day to the track to define the trip)
tacsat$FT_REF <- paste(tacsat$VE_REF, format(as.Date(tacsat$SI_DATE, format = "%d/%m/%Y")+1, format="%d/%m/%Y"), sep="_")
#sort the data by vessel and time
tacsat <- sortTacsat(tacsat) 
tacsat<- intervalTacsat(tacsat,level="trip",fill.na=T)

## Remove pings with interval = 0 and convert NA to 1
tacsat$INTV[is.na(tacsat$INTV)]<-1
tacsat <- tacsat[which(tacsat$INTV>0),]

##Identify intervals between one day and the following and set them to 1
tacsat$INTV_0 <- tacsat$INTV
daydif <- diff(as.Date(tacsat$SI_DATE))
daydif <- c(0, daydif)
tacsat$INTV[daydif>1]<- 1

# INTV between 5 and 30 are set to 5 
intlimit <- 30

length(tacsat$INTV[tacsat$INTV>5 & tacsat$INTV<intlimit])
length(tacsat$INTV[tacsat$INTV>5 & tacsat$INTV<intlimit])/length(tacsat$INTV) #1.8%
tacsat$INTV[tacsat$INTV>5 & tacsat$INTV<intlimit] <- 5

# INTV higher than 30 are identified as holes and are set to 5 so that the do not interfer the catch alocation
length(tacsat$INTV[tacsat$INTV>intlimit])
length(tacsat$INTV[tacsat$INTV>intlimit])/length(tacsat$INTV)
tacsat$hole <- "no"
tacsat$hole[tacsat$INTV>intlimit] <- "yes"
tacsat$INTV[tacsat$INTV>intlimit] <- 5

# Calculate the number of pings per trip
nping_trip<- table(tacsat$FT_REF)
tacsat$Nping_trip <- nping_trip[match(tacsat$FT_REF, names(nping_trip))]


#5. Identify fishing activity ####
#################################################### #
speedmin<- 0.1
speedmax<- 4

tacsat$SI_STATE <- "s"
tacsat$SI_STATE[tacsat$SI_SP>=speedmin & tacsat$SI_SP<speedmax] <- "f"

# Calculate the number of pings per trip with fishing activity
nping_trip<- table(tacsat$FT_REF[tacsat$SI_STATE=="f"])
tacsat$Nping_trip_fish <- nping_trip[match(tacsat$FT_REF, names(nping_trip))]
tacsat$Nping_trip_fish[is.na(tacsat$Nping_trip_fish)] <- 0



############## - Save data - ################
########################################### #

setwd(path)
save(tacsat, tacsat_land, tacsat_har, harbours, coastline_pol, file="Cleantacsat.RData")

load("Cleantacsat.RData")

############## - Summary tables - ################
################################################# #
setwd(res.path)

sumtab<-   ddply(tacsat, .(VE_REF,FT_REF), summarise, 
                 Nping=length(VE_REF), 
                 NpingFishing=length(SI_STATE[SI_STATE=="f"]), 
                 Nhole=length(hole[hole=="yes"]),
                 Timetrack=sum(INTV), 
                 Timefishing=sum(INTV[SI_STATE=="f"]), 
                 Timehole=sum(INTV_0[hole=="yes"]))
write.table(sumtab, "Tacsat trip summary.csv", row.names = FALSE, sep=",", dec=".")


sumtab_vessel<-   ddply(tacsat, .(VE_REF), summarise, 
                 Ntrips=length(unique(FT_REF)), 
#                 Ntrips_more5ping=length(unique(FT_REF[Nping_trip>5])),
                 Ntrips_fishing=length(unique(FT_REF[SI_STATE=="f"])), 
                 Ntrips_fishing_hole=length(unique(FT_REF[SI_STATE=="f"& hole=="yes"])),
                 Ntrips_fishing_more5ping=length(unique(FT_REF[Nping_trip_fish>5])))
write.table(sumtab_vessel, "Tacsat vessel summary.csv", row.names = FALSE, sep=",", dec=".")



############## - Exploratory Plots - ################
######################################## #

  # . plot different ports ----
  ###### #

png(filename="0_Port_Position.png", width=900, height =500)

crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(coastline_pol,  lwd=1, xlim=c(-4,-1), ylim=c(43.2, 43.5))
points(x=as.numeric(harbours$lon),
       y=as.numeric(harbours$lat), col="blue", pch=19)
title("Port Position")

dev.off()


# Next plots are very exahustive. 
# They can be useful for explanatory analysis of the data, but every users should decide whether is needed or not

# . plot tracks for every vessel ---- ** be careful it can be very slow
################################### #

setwd(res.path)

for (vessel in unique(tacsat$VE_REF)){
  
  tacsat_sea_temp <- subset(tacsat, VE_REF==vessel )
  tacsat_land_temp <- subset(tacsat_land, VE_REF==vessel )
  tacsat_har_temp <- subset(tacsat_har, VE_REF==vessel )
  
  ves_ylim <- c(min(tacsat_sea_temp$SI_LATI)-0.01, max(tacsat_sea_temp$SI_LATI)+0.01)
  ves_xlim <- c(min(tacsat_sea_temp$SI_LONG)-0.01, max(tacsat_sea_temp$SI_LONG)+0.01)
  cex.val=0.5

  
#windows()
map1 <- ggplot() +
  ggspatial::geom_spatial(data = coastline_pol, fill = "lightgrey", colour = "black",  cex=cex.val) +
  geom_point(data = tacsat_sea_temp, mapping = aes(x = SI_LONG, y = SI_LATI), col="black", shape=21, cex=cex.val) +
  geom_point(data = tacsat_sea_temp[tacsat_sea_temp$SI_STATE=="f",], mapping = aes(x = SI_LONG, y = SI_LATI), shape=21, col="green", cex=cex.val) +
  geom_point(data = tacsat_land_temp, mapping = aes(x = SI_LONG, y = SI_LATI), col="brown", shape=21, cex=cex.val) +
  geom_point(data = tacsat_har_temp, mapping = aes(x = SI_LONG, y = SI_LATI), col="blue", shape=21, cex=cex.val) +
  geom_point(data = harbours, mapping = aes(x = lon, y = lat), shape=16, col="red", cex=cex.val+2) +
  #theme_void() +
  coord_map(xlim = ves_xlim,ylim = ves_ylim)+
  ggtitle(vessel)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(map1, filename = paste(vessel, ".png"), width = 10, height = 10)
}


# . plot tracks near ports ----
################################### #

# subset harbours
har <- subset(harbours, lon>(-4) & lon<(-1) & lat>43 &lat<43.5 )
# plot
for (port in unique(har$harbour)){
 har_temp <- subset(har, harbour==port )
 xlim <- har_temp$lon+ c(-0.05,0.05)
 ylim <- har_temp$lat+ c(-0.05,0.05)
 cex.val=0.5

  ## subset the traks
 tacsat_sea_port <- subset(tacsat, SI_LONG>xlim[1] & SI_LONG<xlim[2] & SI_LATI>ylim[1] &SI_LATI<ylim[2] )
 
 map2 <-  ggplot() +
    ggspatial::geom_spatial(data = coastline_pol, fill = "lightgrey", colour = "black") +
    geom_point(data = tacsat_sea_port, mapping = aes(x = SI_LONG, y = SI_LATI), shape=21,col="black", cex=cex.val) +
    geom_point(data = tacsat_sea_port[tacsat_sea_port$SI_STATE=="f",], mapping = aes(x = SI_LONG, y = SI_LATI), shape=21, col="green", cex=cex.val) +
    geom_point(data = tacsat_har, mapping = aes(x = SI_LONG, y = SI_LATI), shape=21, col="blue", cex=cex.val) +
    geom_point(data = har, mapping = aes(x = lon, y = lat), shape=16, col="red", cex=cex.val+2) +
    geom_point(data = har_temp, mapping = aes(x = lon, y = lat), shape=16, col="red", cex=cex.val+4) +
   #theme_void() +
    coord_map(xlim = har_temp$lon+ c(-0.05,0.05),ylim = har_temp$lat+ c(-0.05,0.05)) +
    ggtitle(port)+
    theme(plot.title = element_text(hjust = 0.5))
 
 ggsave(map2, filename = paste("port_", port, ".png", sep=""), width = 10, height = 10)
 }


# . plot all trips per vessel ---- ** be careful it can be very slow
################################### #

sort(unique(tacsat$VE_REF))

for (vessel in unique(tacsat$VE_REF)){
  #vessel <- "abelan uno"
  temp <- subset(tacsat, VE_REF==vessel)
  
  cex.val=1

for (trip in unique(temp$FT_REF )){
  
  temp_trip <- subset(temp, FT_REF==trip )

  ves_ylim <- c(min(temp_trip$SI_LATI)-0.01, max(temp_trip$SI_LATI)+0.01)
  ves_xlim <- c(min(temp_trip$SI_LONG)-0.01, max(temp_trip$SI_LONG)+0.01)
  
  map3 <- ggplot() +
    ggspatial::geom_spatial(data = coastline_pol, fill = "lightgrey", colour = "black", cex=cex.val) +
    geom_point(data = temp_trip, mapping = aes(x = SI_LONG, y = SI_LATI), col="black", shape=16, cex=cex.val) +
    geom_point(data = temp_trip[temp_trip$SI_STATE=="f",], mapping = aes(x = SI_LONG, y = SI_LATI), shape=16, col="green", cex=cex.val) +
    geom_point(data = harbours, mapping = aes(x = lon, y = lat), shape=16, col="red", cex=cex.val+2) +
    #theme_void() +
    coord_map(xlim = ves_xlim,ylim = ves_ylim)+
    ggtitle(trip)+
    theme(plot.title = element_text(hjust = 0.5))
  

  speed1<- ggplot(data = temp_trip, mapping = aes(x = SI_DATIM, y = SI_SP), col="black") +
    geom_line()+
    geom_point()+
    geom_point(data = temp_trip[temp_trip$SI_STATE=="f",], mapping = aes(x = SI_DATIM, y = SI_SP), col="green")+
    ylim(0, max(temp_trip$SI_SP))+
    ggtitle(trip)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme(plot.title = element_text(hjust = 0.5))
  
  p1<- grid.arrange(map3, speed1, ncol=2)
  
  ggsave(p1, filename = paste(gsub("/","_", trip), ".png", sep=""), width = 20, height = 10)
}

}


###


