#Leaflet
library(leaflet)
library(htmltools)

sysPa       <-"C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\"
dataPath  <-    paste(sysPa,"Rdata\\",sep="")  
setwd(dataPath)
load("LinkedTacsatEflalo_DATACALL.RData")
pal <- colorFactor(palette = c("red", "blue"), 
                   levels = c(0, 1))

vessel <- AllTacsatEflalo[tacsat$VE_REF1 == "FN168", ]
leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = 10, lat = 56, zoom = 6) %>%
  addCircleMarkers(data=vessel, lng = vessel$SI_LONG, lat = vessel$SI_LATI, radius=1,
                   color=~pal(vessel$SI_STATE)) 
##MAPPING A SUBSET OF GILLNET TRIPS#
GNS <- head(AllTacsatEflalo[tacsat$LE_GEAR == "GNS" , ],10000)
GNS_FISHING  <- GNS[GNS$SI_STATE == 1, ]  
GNS_STEAMING <- GNS[GNS$SI_STATE == 0, ]  

leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = 10, lat = 56, zoom = 6) %>%
  addCircleMarkers(data=GNS_FISHING, lng = GNS_FISHING$SI_LONG, lat = GNS_FISHING$SI_LATI, radius=1,
                   color="blue", group="GNS Fishing", popup=~FT_REF ) %>%
  addCircleMarkers(data=GNS_STEAMING, lng = GNS_STEAMING$SI_LONG, lat = GNS_STEAMING$SI_LATI, radius=1,
                 color="red", group="GNS Steaming", popup=~FT_REF) %>%
  addLayersControl(overlayGroups = c("GNS Fishing","GNS Steaming")) 


##MAPPING SPEED AS CONTINOUS VARIABLE#
pal <- colorNumeric(palette = "RdBu", domain = c(0:10))

leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = 10, lat = 56, zoom = 6) %>%
  addCircleMarkers(data=GNS, lng = GNS$SI_LONG, lat = GNS$SI_LATI, radius=1,
                   color=~pal(SI_SP), group="GNS Speed", popup=~FT_REF)

  