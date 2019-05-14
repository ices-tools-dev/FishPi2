
# R  3.2.3


# -- Load libraries---- ####

# vmstoolsPackages <-  c("cluster","data.table","doBy","maps","mapdata","maptools","PBSmapping","sp")
# 
# for(i in vmstoolsPackages)       try(install.packages(pkgs=i,repos=getOption("repos")))
#install.packages("C:\\use\\BATEGIN\\BATEGIN II\\Pruebas AIS prueba\\vmstools_0.73.zip") por si no lo hace automaticamente

library(vmstools)
library(reshape2)



# -- Read data ----  ####

path<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Data"

setwd(path)

logbook <-read.csv("Capturas 2017 flota AIS B_v2.csv", dec=",", sep=";", header=T, stringsAsFactors = FALSE)
vms <- read.csv("AIS_pescapv_2017.txt", sep=",",dec=".",header=T, stringsAsFactors = FALSE)
PortPosition <-read.csv("port position.csv", sep=",",header=T, stringsAsFactors = FALSE)



# 1.	Convert catch data into EFLALO format. ####
############################################## #

head(logbook); tail(logbook)
str(logbook)
names(logbook)
sum(logbook$Kg_Desemb_Peso_Vivo, na.rm=TRUE) # 2,201,640 kg



#  -- Filter by vessel name --  

sort(unique(vms$nombre))
sort(unique(logbook$Nombre_Buque))
vms$nombre <- tolower(vms$nombre)
logbook$Nombre_Buque <- tolower(logbook$Nombre_Buque)

vms$nombre[vms$nombre=="gure naiara"] <- "gure naiara"
logbook$Nombre_Buque[logbook$Nombre_Buque=="gure naiara (ex. ana karina)" ] <- "gure naiara"
vms$nombre[vms$nombre=="itxasoko loreak dos "] <- "itxasoko lorea dos" 
vms$nombre[vms$nombre=="jon kurtsio"] <- "jon kurtzio" 
logbook$Nombre_Buque[logbook$Nombre_Buque=="jon kurtzio (ex artxanda eder)"] <- "jon kurtzio"
vms$nombre[vms$nombre=="kalaberri"] <- "kala berri" 
vms$nombre[vms$nombre=="laura y cristina"] <- "beti zeruko izarra"
logbook$Nombre_Buque[logbook$Nombre_Buque=="beti zeruko izarra (ex laura y cristina)"] <- "beti zeruko izarra"
vms$nombre[vms$nombre=="portu zarra primero" ] <- "portuzarra primero" 
vms$nombre[vms$nombre=="bihotz  alai" ] <- "bihotz alai"
logbook$Nombre_Buque[logbook$Nombre_Buque=="gure ametxa (ex. totaio)"] <- "gure ametxa"
vms$nombre[vms$nombre=="totaio" ] <- "gure ametxa"


vms<- subset(vms, !nombre %in% c("mar de llanes","miren argia", "romu ",  "sabadeo"))
logbook<- subset(logbook, Nombre_Buque %in% unique(vms$nombre))



 
# --- Transform variables from sales notes to eflalo
  
logbook$Fecha <- ISOdate(logbook$Ano,logbook$Mes, logbook$Dia )
logbook$Month <- months(logbook$Fecha)
logbook$Week<-as.numeric(format(logbook$Fecha, format="%W"))

logbook$VE_REF<-tolower(logbook$Nombre_Buque)
logbook$VE_FLT<-logbook$Metier
logbook$VE_COU<-"ESP"
logbook$VE_LEN<-logbook$Eslora_total
logbook$VE_KW<-logbook$Potencia_KW
logbook$VE_TON<-NA
logbook$FT_DCOU<-"ESP"
logbook$FT_DHAR<-logbook$Puerto_Venta #no tenemos esta información. ponemos el mismo puerto que desembarco
logbook$FT_DDAT <- format(as.POSIXct(logbook$Fecha, format = "%Y-%m-%d"),  "%d/%m/%Y")
logbook$FT_REF<- paste(logbook$VE_REF,logbook$FT_DDAT, sep="_") # este campo define la marea

logbook$FT_DTIME<-"02:00"
logbook$FT_LCOU<-"ESP"
logbook$FT_LHAR<-logbook$Puerto_Venta
logbook$FT_LDAT<-logbook$FT_DDAT
logbook$FT_LTIME<-"23:00"
logbook$LE_ID<-paste(logbook$VE_REF,logbook$FT_DDAT, sep="_")  # este campo define l lance

logbook$LE_CDAT<-logbook$FT_DDAT
logbook$LE_GEAR<-substr(logbook$Metier,1,3)
logbook$LE_MSZ<-NA
logbook$LE_RECT<-NA
logbook$LE_DIV<-logbook$Zona
logbook$LE_MET<-logbook$Metier
logbook$LE_SP<-logbook$Especie_ALFA3
logbook$LE_SPW<-logbook$Kg_Desemb_Peso_Vivo
logbook$LE_EURO<-logbook$Precio_Total


# -- build eflalo
eflalo_ini<- logbook[,c("VE_REF", "VE_FLT" , "VE_COU", "VE_LEN",  "VE_KW", "VE_TON",  "FT_DCOU", "FT_DHAR",  
                        "FT_DDAT", "FT_REF",  "FT_DTIME",  "FT_LCOU", "FT_LHAR",  "FT_LDAT", "FT_LTIME",  
                        "LE_ID",  "LE_CDAT", "LE_GEAR", "LE_MSZ", "LE_RECT", "LE_DIV", "LE_MET", "LE_SP", 
                        "LE_SPW", "LE_EURO")]


eflalo_ini$LE_SP<- paste("LE_KG_",eflalo_ini$LE_SP, sep="")
eflalo_cast <- dcast(eflalo_ini, VE_REF +VE_FLT +VE_COU +VE_LEN + VE_KW +VE_TON +FT_REF +FT_DCOU +FT_DHAR+
                      FT_DDAT+ FT_DTIME+ FT_LCOU+ FT_LHAR+ FT_LDAT+ FT_LTIME+ LE_ID+ LE_CDAT+ LE_GEAR+ LE_MSZ+ 
                      LE_RECT+ LE_DIV+ LE_MET  ~ LE_SP,fill=0,value.var = "LE_SPW") #el fill=0 hace que en vez de poner NA a las celdas vacías las rellene como = 0 que es lo real.


eflalo_art <-eflalo_cast
      
head(eflalo_art); dim(eflalo_art)    

 sum(eflalo_art [23:128]) # 2,201,640 # Comprobado = que valor salida BD.




# 2.	Convert geospatial data into TACSAT format ####
##################################### #


tacsat_art <- vms  

head(tacsat_art)


tacsat_art$VE_REF <- tolower(tacsat_art$nombre)
tacsat_art$SI_DATE <- format(as.POSIXct(tacsat_art$dia, format = "%Y-%m-%d"),  "%d/%m/%Y")
tacsat_art$SI_TIME <- format(as.POSIXct(tacsat_art$hora, format = "%H:%M:%S"),  "%H:%M")
tacsat_art$SI_LATI <- tacsat_art$latitud
tacsat_art$SI_LONG <- tacsat_art$longitud
tacsat_art$SI_SP <- tacsat_art$sog
tacsat_art$SI_HE <- NA

tacsat_art <- tacsat_art[,10:16]



#  Save tacsat and eflalo objects
##################################################### #

save(eflalo_art,tacsat_art, file="Datos_art_2017_trip.RData")



#write.table(eflalo_art, "eflalo_art.csv", sep=",", row.names=F)
#write.table(tacsat_art, "tacsat_art.csv",sep=",",row.names=F)


