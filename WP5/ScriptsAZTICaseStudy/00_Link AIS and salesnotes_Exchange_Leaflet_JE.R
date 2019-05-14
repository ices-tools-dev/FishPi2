
# R 3.4.4 
######


rm(list=ls())
library(sp)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)


# Set paths
############################################
path<- "C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\Rdata\\"
path.res<- "C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\Results\\"


# Load data
############################################
setwd(path)

load("Cleantacsat.RData")
load("eflaloClean_2017.RData")
#load("eurPols.RData")


# 0. Create final subset 
#############################################################################################

# remove tacsat trips with no fishing 
############################################
id <- unique(tacsat$FT_REF[which(tacsat$SI_STATE=="1")])
tacsat <- subset(tacsat, tacsat$FT_REF %in% id)

# remove tacsat with less or equal than 5 pings
############################################
id <- unique(tacsat$FT_REF[which(tacsat$Nping_trip>=5)])
tacsat <- subset(tacsat, tacsat$FT_REF %in% id)

# clean gear
############################################
unique(eflalo$LE_GEAR)
eflalo$LE_GEAR[eflalo$LE_GEAR %in% c("BMS", "DRC", "DRO")] <- "DRB"


# 1. merge vms and logbook
##############################################################################################
#  I have used a loop instead of the function mergeEflalo2Tacsat, to have more control on the link
#  Algorithm:
#            date tacsat + 1                =   date eflalo
#            date tacsat[not matched]       =   date eflalo
#            date tacsat[not matched] + 2   =   date eflalo
#            date tacsat[not matched] + 3   =   date eflalo
#            date tacsat[not matched] + 4   =   date eflalo
#####################################################################################################
setwd(path.res)

# Link trips
###############################################################################################

uniquetacsat <- unique(tacsat[, c("VE_REF1", "SI_DATE", "FT_REF")])
maxTime<-   ddply(tacsat, .(FT_REF), summarise, maxTime=max(SI_TIME))
uniquetacsat$maxTime <- maxTime$maxTime[match(uniquetacsat$FT_REF, maxTime$FT_REF)]
uniquetacsat$id <- "tacsat"

uniqueeflalo <- unique(eflalo[, c("VE_REF1", "FT_LDAT", "FT_REF", "LE_GEAR")])
names(uniqueeflalo)[names(uniqueeflalo)=="FT_LDAT"] <- "SI_DATE"
uniqueeflalo$id <- "eflalo"

uniquetacsat$FT_REF_eflalo<- NA
uniquetacsat$link0 <-NA
for(i in c(1,0,2, 3,4)){
  uniquetacsat$link0<-paste(uniquetacsat$VE_REF1,format(as.Date(uniquetacsat$SI_DATE,tz = "CET","%d/%m/%Y")+i,format = "%d/%m/%Y"),sep="_")
  uniquetacsat$FT_REF_eflalo[is.na(uniquetacsat$FT_REF_eflalo)] <- uniqueeflalo$FT_REF[match(uniquetacsat$link0[is.na(uniquetacsat$FT_REF_eflalo)], uniqueeflalo$FT_REF)]
}

uniquetacsat <- uniquetacsat[order(uniquetacsat$VE_REF1, as.Date(uniquetacsat$SI_DATE, format = "%d/%m/%Y")),]
uniquetacsat <- subset(uniquetacsat, select=- link0)
uniquetacsat$LE_GEAR <- uniqueeflalo$LE_GEAR[match(uniquetacsat$FT_REF_eflalo, uniqueeflalo$FT_REF)]  # FT_REF is now the code for linked trips


tacsat$FT_REF_0 <- tacsat$FT_REF  # FT_REF_0 is now the trip code for all tacsat trips
tacsat$FT_REF <- uniquetacsat$FT_REF_eflalo[match(tacsat$FT_REF_0, uniquetacsat$FT_REF)]  # FT_REF is now the code for linked trips
tacsat$FT_REF[is.na(tacsat$FT_REF)] <- 0


  # 2. adding variables: gear and harbour
############################################

tacsat$LE_GEAR <- eflalo$LE_GEAR[match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$FT_LHAR <- eflalo$FT_LHAR[match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$VE_LEN  <- eflalo$VE_LEN[match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$VE_KW   <- eflalo$VE_KW[match(tacsat$FT_REF,eflalo$FT_REF)]

table(tacsat$LE_GEAR)
table(tacsat$FT_LHAR)


## 3. Split catches among pings. 
#     This code allows:
#         a) splitting catches among trips (just uses linked trips) 
#         b) splitting catches of the combination vessel+month+gear among corresponding pings
#
##########################################################



 # link trip (vessel day) 
  tacsat_ef<- tacsat %>% mutate(effishing=ifelse(SI_STATE=="1",INTV,0))%>%
          group_by(FT_REF) %>%
          mutate(sumf=sum(effishing),propef=effishing/sumf)%>%
          ungroup() %>%
          mutate(month=months(as.Date(SI_DATE,tz = "CET","%d/%m/%Y")),  # proportions for a) splitting among trips
                 FT_REF_MONTH=paste(VE_REF1, month,LE_GEAR, sep="_"))  %>%
          group_by(FT_REF_MONTH) %>%
          mutate(sumfm=sum(effishing),propefm=effishing/sumfm)%>%       # proportions for b) splitting among vessel+month+gear
          ungroup()

  cols_LE_KG <- colnames(eflalo %>% dplyr:: select(starts_with("LE_KG")))
  cols_LE_EURO <- colnames(eflalo %>% dplyr:: select(starts_with("LE_EURO")))
  eflalo$LE_KG_TOTAL <- rowSums(eflalo[, ..cols_LE_KG], na.rm = TRUE)
  eflalo$LE_EURO_TOTAL <- rowSums(eflalo[, ..cols_LE_EURO], na.rm = TRUE)
  
  cols <- c("FT_REF","LE_KG_TOTAL","LE_EURO_TOTAL")

  simpw<-eflalo[, ..cols]%>%
                group_by(FT_REF)%>%
                summarise_all(sum,na.rm=T)%>%
                ungroup()

  tacsatEflalo <- tacsat_ef %>%  left_join(simpw,by=c("FT_REF"="FT_REF")) %>%
    mutate_at(.funs = funs(. * propef), .vars = vars(contains("LE_KG")))  %>%
    mutate_at(.funs = funs(. * propef), .vars = vars(contains("LE_EURO"))) 

  # link vessel month
  eflalo<- eflalo %>% mutate(month=months(as.Date(FT_DDAT,tz = "CET","%d/%m/%Y")),
                             FT_REF_MONTH=paste(VE_REF1, month, LE_GEAR, sep="_"))  
  
  simpw2<- eflalo %>% subset(select=c("FT_REF_MONTH", colnames(eflalo)[grep("LE_KG", colnames(eflalo))]))  %>%
                               group_by(FT_REF_MONTH)  %>%
                               summarise_all(sum,na.rm=T)  %>%
                               ungroup()
  
  tacsatEflalo_month <- tacsatEflalo %>% subset(!is.na(LE_GEAR))  %>%
    subset(select=c( colnames(tacsatEflalo)[-grep("LE_KG", colnames(tacsatEflalo))]))  %>%
    left_join(simpw2,by=c("FT_REF_MONTH"="FT_REF_MONTH")) %>% 
    mutate_at(.funs = funs(. * propefm), .vars = vars(contains("LE_KG")))
  


# Some chekings  
  sum(eflalo$LE_KG_TOTAL, na.rm=T)
  sum(eflalo$LE_KG_TOTAL[eflalo$FT_REF %in% unique(tacsat_ef$FT_REF)], na.rm=T)
  sum(tacsatEflalo$LE_KG_TOTAL, na.rm=T)
  sum(tacsatEflalo_month$LE_KG_TOTAL, na.rm=T)
  sum(tacsatEflalo$LE_EURO_TOTAL, na.rm=T)
  sum(tacsatEflalo_month$LE_EURO_TOTAL, na.rm=T)


     # Create List with all GEARS
   ##### 
  AllTacsatEflalo <- list()
  AllTacsatEflalo_month <- list()
  gearlist <- unique(tacsat$LE_GEAR[!is.na(tacsat$LE_GEAR)])
  
  for (gear in gearlist){
   AllTacsatEflalo[[which(gearlist==gear)]] <- subset(tacsatEflalo,LE_GEAR==gear)
   AllTacsatEflalo_month[[which(gearlist==gear)]] <- subset(tacsatEflalo_month,LE_GEAR==gear)
  }

names (AllTacsatEflalo) <- gearlist
names (AllTacsatEflalo_month) <- gearlist
lapply(AllTacsatEflalo, dim)
lapply(AllTacsatEflalo_month, dim)

# Save results
setwd(path)
save(tacsat, tacsat_ef, eflalo, AllTacsatEflalo, AllTacsatEflalo_month, harbours, coastline_pol, file="LinkedTacsatEflalo.RData")



# # Create a detailed table to explore why some trips were not linked
# ############################################################################
link_id <- uniquetacsat$FT_REF_eflalo[which(!is.na(uniquetacsat$FT_REF_eflalo))]
eflalo_nolink <- uniqueeflalo[which(!uniqueeflalo$FT_REF %in% link_id),]

alltrips<- rbind.fill(uniquetacsat,eflalo_nolink)
alltrips <- alltrips[order(alltrips$VE_REF1, as.Date(alltrips$SI_DATE, format = "%d/%m/%Y")),]

setwd(path.res)
write.table(alltrips, "0_Linked_Trips_detail.csv", row.names = FALSE, sep=",", dec=".")

#############################################################################################
#############################################################################################
## JE: SUM DATA BY 0.01 DEGREES C_SQUARE##
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

