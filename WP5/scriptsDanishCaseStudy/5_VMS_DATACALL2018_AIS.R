library(vmstools)
require(sp)
library(Matrix)   #- available on CRAN
library(ggplot2)  #- available on CRAN
library(data.table)



#- Set the working directory to the folder where you keep your code and data

sysPa       <-"C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\"
dataPath  <-    paste(sysPa,"Rdata\\",sep="")  
setwd(dataPath)
load("LinkedTacsatEflalo.RData")
AisExchange <- AllTacsatEflalo

#- Settings paths
outPath   <- paste0(sysPa,"Results/")

#- Set the country abbreviation to e.g. deu, gbr, dnk
country     <- "DNK"
interval    <- 5 #set interval time applicable for your country VMS

  Year     <- 2017
  tacsat <- data.frame(tacsat) # format each of the columns to the specified class
  eflalo <- data.frame(eflalo) # format each of the columns to the specified class
  
  #-------------------------------------------------------------------------------
  #- Calculate effort (days)
  #-------------------------------------------------------------------------------
  eflalo$INTV           <- difftime(eflalo$FT_LDATIM,eflalo$FT_DDATIM,units="days")
  eflalo$dummy          <- 1
  res                   <- aggregate(eflalo$dummy,by=list(eflalo$FT_REF),FUN=sum,na.rm=T)
  colnames(res)         <- c("FT_REF","nrRecord")
  eflalo                <- merge(eflalo,res,by="FT_REF")
  eflalo$INTV           <- eflalo$INTV / eflalo$nrRecord
  eflalo$LE_KG_DAS      <- eflalo$INTV
  
  #- Merge eflalo and tacsat together
  tacsatp               <- mergeEflalo2Tacsat(eflalo,tacsat)
  #- Assign gear and length to tacsat
  tacsatp$LE_GEAR       <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$VE_LEN        <- eflalo$VE_LEN[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$LE_MSZ        <- eflalo$LE_MSZ[ match(tacsatp$FT_REF,eflalo$FT_REF)]
  
  
  #- Only keep Eflalo gears from tacsat
  tacsatp               <- subset(tacsatp,FT_REF != 0)
  #- Get time between pings
  tacsatp               <- intervalTacsat(tacsatp,level="trip",fill.na=TRUE)  ## Changes high INTV values in the start of the trip to the same as the INTV in the second ping
  tacsatp$INTV[is.na(tacsatp$INTV)] <- interval
  tacsatp$INTV[tacsatp$INTV>10]    <- interval  ### If there is no pings for more than two hours, set the interval value to one hour
  eflalo$INTV           <- as.numeric(eflalo$INTV*24*60)
  eflalo$LE_KG_DAS      <- as.numeric(eflalo$LE_KG_DAS)
  eflalo$INTV          <- round(eflalo$INTV/60, 2)*60  ## Get the same difftime as the sas procedure.
  eflalo$LE_KG_DAS          <- round(eflalo$LE_KG_DAS/60, 2)*60  ## Get the same difftime as the sas procedure.
  
  
  #- Define activitity based on vesselspeed
  tacsatp               <- tacsatp[!is.na(tacsatp$SI_SP),]
  
  #- Read in fixed speed bounds for different gears
  # !!! Make sure that all your gears are added to this speedarr.csv list, some may be lacking !!! #
  
  speedarr              <- read.csv(file=paste(dataPath,"/speedarr.csv",sep=""))
  colnames(speedarr)    <- c("LE_GEAR","min","max")
  speedarr$LE_GEAR      <- ac(speedarr$LE_GEAR)
  speedarr              <- speedarr[speedarr$LE_GEAR%in%unique(tacsatp$LE_GEAR),]
  speedarr              <- speedarr[!duplicated(speedarr$LE_GEAR),]
  
  tacsatp <- tacsatp[tacsatp$LE_GEAR%in%speedarr$LE_GEAR,]
  
  #-------------------------------------------------------------------------------
  #- Assign the activity per gear
  #-------------------------------------------------------------------------------
  gears                 <- unique(tacsatp$LE_GEAR)
  tacsatp$SI_STATE<-0
  for (mm in gears) {
    tacsatp$SI_STATE[tacsatp$LE_GEAR==mm& tacsatp$SI_SP >= an(speedarr[speedarr$LE_GEAR==mm,"min"])& tacsatp$SI_SP <= an(speedarr[speedarr$LE_GEAR==mm,"max",])]<-1
  }
 
  tacsatp               <- tacsatp[!is.na(tacsatp$INTV),]
 
  #-------------------------------------------------------------------------------
  #- 6) Dispatch landings of merged eflalo at the ping scale
  #-------------------------------------------------------------------------------
  idxkgeur          <- kgeur(colnames(eflalo))
  eflalo$LE_KG_TOT  <- rowSums(eflalo[,grep("LE_KG_",colnames(eflalo))],na.rm=T)
  eflalo$LE_EURO_TOT<- rowSums(eflalo[,grep("LE_EURO_",colnames(eflalo))],na.rm=T)
  eflalo            <- eflalo[,-idxkgeur]
  eflaloNM          <- subset(eflalo,!FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM           <- subset(eflalo,FT_REF %in% unique(tacsatp$FT_REF))
  
 
  #- Split eflalo up in two sets, one set that cannot be merged to tacsat and one which matches
  eflaloNM              <- subset(eflalo,!FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM               <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  
  #Disable the function in splitAmongPings that assigns 0 values to rectangles that is not in the eflalo
  tacsatp$LE_RECT         <- "ALL"
  eflaloM$LE_RECT         <- "ALL"
  
  #Throw away catches that has no matching vms points
  tacsatp$FT_REF        <- paste(tacsatp$FT_REF, tacsatp$SI_DATE, sep = "_")
  eflaloM$FT_REF        <- paste(eflaloM$FT_REF, eflaloM$LE_CDAT, sep = "_")
  eflaloNM$FT_REF        <- paste(eflaloNM$FT_REF, eflaloNM$LE_CDAT, sep = "_")
  eflalo$FT_REF        <- paste(eflalo$FT_REF, eflalo$LE_CDAT, sep = "_")
  eflaloM <- eflalo[!is.na(eflaloM$INTV),]
  tacsatp <- tacsatp[tacsatp$INTV > 0,]

  #- Split effort and total landing among pings
  tacsatEflalo          <- (splitAmongPings(tacsat=subset(tacsatp,FT_REF %in% unique(eflaloM$FT_REF)),eflalo=subset(eflaloM),variable="all" ,level="trip",by="INTV",conserve=F))
  cat("total value in eflalo\n")
  print(colSums(eflaloM[c(grep("KG",colnames(eflaloM)))],na.rm=T))
  cat("total value in tacsatEflalo\n")
  print(colSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T))
  cat("ratio total value\n")
  print(colSums(eflaloM[c(grep("KG",colnames(eflaloM)))],na.rm=T)/ colSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T))
  
  
  tacsatEflalo$Csquare  <- CSquare(tacsatEflalo$SI_LONG,tacsatEflalo$SI_LATI,degrees=0.01)
  tacsatEflalo$Year     <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month    <- month(tacsatEflalo$SI_DATIM)
  tacsatEflalo$kwHour   <- tacsatEflalo$VE_KW * tacsatEflalo$INTV/60
  tacsatEflalo$INTV     <- tacsatEflalo$INTV/60
  tacsatEflalo$LENGTHCAT<- cut(tacsatEflalo$VE_LEN,breaks=c(0,8,10,12,15,200))
  tacsatEflalo$LENGTHCAT<- ac(tacsatEflalo$LENGTHCAT)
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(0,8]")]   <- "<8"
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(8,10]")]   <- "8-10"
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(10,12]")]   <- "10-12"
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(12,15]")]  <- "12-15"
  tacsatEflalo$LENGTHCAT[which(tacsatEflalo$LENGTHCAT == "(15,200]")] <- ">15"
  
  RecordType <- "VE"
  
  setwd(dataPath)
  save(tacsatEflalo, file="LinkedTacsatEflalo_DATACALL.RData")
  
  
  table1                <- cbind(RT=RecordType,tacsatEflalo[,c("VE_COU","Year","Month","Csquare","LENGTHCAT","LE_GEAR","LE_MET","SI_SP","INTV","VE_LEN","VE_KW","LE_KG_TOT","LE_EURO_TOT")])
  table1Sums              <- aggregate(table1[,c("INTV","LE_KG_TOT","LE_EURO_TOT")],
                                       by=as.list(table1[,c("RT","VE_COU","Year","Month","Csquare","LENGTHCAT","LE_GEAR","LE_MET")]),
                                       FUN=sum,na.rm=T)
  table1Means             <- aggregate(table1[,c("SI_SP","VE_LEN","VE_KW")],
                                       by=as.list(table1[,c("RT","VE_COU","Year","Month","Csquare","LENGTHCAT","LE_GEAR","LE_MET")]),
                                       FUN=mean,na.rm=T)
  table1Save              <- cbind(table1Sums,table1Means[,c("SI_SP","VE_LEN","VE_KW")])
  colnames(table1Save)    <- c("RecordType","VesselFlagCountry","Year","Month","C-square","LengthCat","Gear","Europeanlvl6","Fishing hour","TotWeight","TotEuro","Av fish speed","Av vessel length","Av vessel KW")
  
  
  #-------------------------------------------------------------------------------
  #- 8) Assign  year, month, quarter, area and create table 2
  #-------------------------------------------------------------------------------
  
  eflalo$Year             <- year(eflalo$FT_LDATIM)
  eflalo$Month            <- month(eflalo$FT_LDATIM)
  eflalo$INTV             <- 1 #1 day
  eflalo$dummy            <- 1
  res                     <- aggregate(eflalo$dummy,by=as.list(eflalo[,c("VE_COU","VE_REF","LE_CDAT")]),FUN=sum,na.rm=T)
  colnames(res)           <- c("VE_COU","VE_REF","LE_CDAT","nrRecords")
  eflalo                  <- merge(eflalo,res,by=c("VE_COU","VE_REF","LE_CDAT"))
  eflalo$INTV             <- eflalo$INTV / eflalo$nrRecords
  eflalo$tripInTacsat     <- ifelse(eflalo$FT_REF %in% tacsatp$FT_REF,"Yes","No")
  
  eflalo$LENGTHCAT        <- cut(eflalo$VE_LEN,breaks=c(0,8,10,12,15,200))
  eflalo$LENGTHCAT        <- ac(eflalo$LENGTHCAT)
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(0,8]")]   <- "<8"
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(8,10]")]  <- "8-10"
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(10,12]")]  <- "10-12"
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(12,15]")]  <- "12-15"
  eflalo$LENGTHCAT[which(eflalo$LENGTHCAT == "(15,200]")] <- ">15"
  
  RecordType <- "LE"
  
  table2                <- cbind(RT=RecordType,eflalo[,c("VE_COU","Year","Month","LE_RECT","LE_GEAR","LE_MET","LENGTHCAT","tripInTacsat","INTV","LE_KG_TOT","LE_EURO_TOT")])
  table2Save              <- aggregate(table2[,c("INTV","LE_KG_TOT","LE_EURO_TOT")],
                                       by=as.list(table2[,c("RT","VE_COU","Year","Month","LE_RECT","LE_GEAR","LE_MET","LENGTHCAT","tripInTacsat")]),
                                       FUN=sum,na.rm=T)
  colnames(table2Save)    <- c("RecordType","VesselFlagCountry","Year","Month","ICESrect","Gear","Europeanlvl6","LengthCat","VMS enabled","FishingDays","TotWeight","TotValue")

GearChange <- c("BMS", "DRC", "DRO")

table1Save$Gear[table1Save$Gear%in%GearChange] <- "DRB"
table2Save$Gear[table2Save$Gear%in%GearChange] <- "DRB"


outPath3 <- "C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\Results\\"


fwrite(table1Save,file.path(outPath3,"table1.csv"))
fwrite(table2Save,file=file.path(outPath3,"table2.csv"))

