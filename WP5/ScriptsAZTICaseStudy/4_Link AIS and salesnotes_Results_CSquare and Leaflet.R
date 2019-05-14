
# R 3.4.4 
####


rm(list=ls())
library(sp)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)



# Set paths ----
########################################### #
path<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Data"
path.res<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Results\\_trip"


# Load result files ----
########################################## #
setwd(path)

load("LinkedTacsatEflalo.RData")





## JE: Sum data by 0.01 degrees CSquare ----
############################################## #

library(vmstools)


tacsatEflalo$Year     <- year(tacsatEflalo$SI_DATIM)
tacsatEflalo$Month    <- month(tacsatEflalo$SI_DATIM)
tacsatEflalo$kwHour   <- tacsatEflalo$VE_KW * tacsatEflalo$effishing/60
tacsatEflalo$FISHING_HOURS     <- tacsatEflalo$effishing/60
tacsatEflalo$LENGTHCAT<- cut(tacsatEflalo$VE_LEN,breaks=c(0,8,10,12,15,200))
tacsatEflalo$LENGTHCAT<- ac(tacsatEflalo$LENGTHCAT)
tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(0,8]")]   <- "<8"
tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(8,10]")]   <- "8-10"
tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(10,12]")]   <- "10-12"
tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(12,15]")]  <- "12-15"
tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(15,200]")] <- ">15"
tacsatEflalo$Csquare  <- CSquare(tacsatEflalo$SI_LONG,tacsatEflalo$SI_LATI,degrees=0.01)

tacsatEflalo$RecordType <- "VE"
tacsatEflalo$VE_COU="DNK"

tacsatEflaloFishing <- tacsatEflalo[tacsatEflalo$SI_STATE=="1",]

table1                <- cbind(tacsatEflaloFishing[,c("RecordType","VE_COU","Year","Month","Csquare","LENGTHCAT","LE_GEAR","SI_SP","VE_LEN","VE_KW","FISHING_HOURS","kwHour","LE_KG_TOTAL","LE_EURO_TOTAL")])

table1Sums              <- aggregate(table1[,c("FISHING_HOURS","kwHour","LE_KG_TOTAL","LE_EURO_TOTAL")],
                                     by=as.list(table1[,c("RecordType","VE_COU","Year","Month","Csquare","LENGTHCAT","LE_GEAR")]),
                                     FUN=sum,na.rm=T)
table1Means             <- aggregate(table1[,c("SI_SP","VE_LEN","VE_KW")],
                                     by=as.list(table1[,c("RecordType","VE_COU","Year","Month","Csquare","LENGTHCAT","LE_GEAR")]),
                                     FUN=mean,na.rm=T)
table1Save              <- cbind(table1Sums,table1Means[,c("SI_SP","VE_LEN","VE_KW")])

#####################################################
#Leaflet
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(raster)

# make raster
resolution <- 0.01
loc <- as.matrix(vmstools::CSquare2LonLat(table1Save$Csquare, degrees = resolution))[,2:1]
colnames(loc) <- c('X', 'Y')

# set up an 'empty' raster, here via an extent object derived from your data
r <- raster(extent(loc)  + resolution/2,
            resolution = resolution,
            crs = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##############################
#LEAFLET TOTAL FISHING HOURS, TOTAL KG, TOTAL EURO
r_fishing_hours <- rasterize(loc, r, table1Save$FISHING_HOURS, fun = "sum")
r_total_kg <- rasterize(loc, r, table1Save$LE_KG_TOTAL, fun = "sum")
r_total_euro <- rasterize(loc, r, table1Save$LE_EURO_TOTAL, fun = "sum")

pal_blue <- colorNumeric(palette = "Greens", na.color = NA, domain=NULL)
pal_green <- colorNumeric(palette = "Blues", na.color = NA, domain=NULL)
pal_orange <- colorNumeric(palette = "OrRd", na.color = NA, domain=NULL)

m <- 
  leaflet() %>% 
  setView(lng = 10, lat = 56, zoom = 6) %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.OceanBasemap)

# add layers
m <- addRasterImage(m, r_fishing_hours, colors=pal_orange, group="Total_fishing_hours", opacity = 0.8) %>%
     addRasterImage(r_total_kg, colors=pal_green, group="Total_kg", opacity = 0.8) %>%
     addRasterImage(r_total_euro, colors=pal_blue, group="Total_euro", opacity = 0.8) %>%
     addLayersControl(overlayGroups = c("Total_fishing_hours","Total_kg","Total_euro"))

m
saveWidget(m, file=paste0(gsub(" ", "_", "Total_fishing_hours_euro_kg"), ".html"), selfcontained = TRUE, title = "Total_fishing_hours_euro_kg")
##############################
#LEAFLET TOTAL FISHING HOURS BY GEAR GROUP
gearlist

table1Save$SI_LONG2 <- CSquare2LonLat(table1Save$Csquare,0.01)$SI_LONG
table1Save$SI_LATI2 <- CSquare2LonLat(table1Save$Csquare,0.01)$SI_LATI


table1BottomTrawl <- table1Save[table1Save$LE_GEAR %in% c("OTB", "PTB"), ]
table1Gillnet <- table1Save[table1Save$LE_GEAR %in% c("GNS", "GTR","GN"), ]
table1PotsTraps <- table1Save[table1Save$LE_GEAR %in% c("FPO", "FPN"), ]
table1Longlines <- table1Save[table1Save$LE_GEAR %in% c("LL", "LLD","LLS"), ]
table1PelagicTrawl <- table1Save[table1Save$LE_GEAR %in% c("PTM", "OTM"), ]
table1Dredge <- table1Save[table1Save$LE_GEAR %in% c("DRB"), ]


m <- 
  leaflet() %>% 
  setView(lng = 10, lat = 56, zoom = 6) %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(data=table1BottomTrawl, lng = table1BottomTrawl$SI_LONG2, lat = table1BottomTrawl$SI_LATI2, radius=1, color="Orange",group="Bottom_trawl") %>%
  addCircleMarkers(data=table1Gillnet, lng = table1Gillnet$SI_LONG2, lat = table1Gillnet$SI_LATI2, radius=1, color="Blue",group="Gillnet") %>%
  addCircleMarkers(data=table1PotsTraps, lng = table1PotsTraps$SI_LONG2, lat = table1PotsTraps$SI_LATI2, radius=1, color="Green",group="Pots_Traps") %>%
  addCircleMarkers(data=table1Longlines, lng = table1Longlines$SI_LONG2, lat = table1Longlines$SI_LATI2, radius=1, color="Red",group="Longlines") %>%
  addCircleMarkers(data=table1PelagicTrawl, lng = table1PelagicTrawl$SI_LONG2, lat = table1PelagicTrawl$SI_LATI2, radius=1, color="Black",group="PelagicTrawl") %>%
  addCircleMarkers(data=table1Dredge, lng = table1Dredge$SI_LONG2, lat = table1Dredge$SI_LATI2, radius=1, color="Grey",group="Dredge") %>%
  addLayersControl(overlayGroups = c("Bottom_trawl","Gillnet","Pots_Traps","Longlines","PelagicTrawl","Dredge"))
m
saveWidget(m, file=paste0(gsub(" ", "_", "Gears"), ".html"), selfcontained = TRUE, title = "Gears")

