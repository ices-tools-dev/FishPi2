
# R 3.4.4 
######

library(vmstools)
library(RColorBrewer)
library(sp)
library(lubridate)
library(mapdata)
library(reshape2)
library(plyr)
library(rgdal)

# Set paths
############################################
#################################### Esta parte la necesito?? preguntar a Lucia
sysPa       <-"C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\"
dataPath   <-    paste(sysPa,"Rdata\\",sep="")   
res.path   <- "C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\Results"


# Load data
############################################
setwd(dataPath)

load("Cleantacsat.RData")
load("eflaloClean_2017.RData")

tacsat <- data.frame(tacsat) # format each of the columns to the specified class
eflalo <- data.frame(eflalo) # format each of the columns to the specified class

#-------------------------------------------------------------------------------
#- Calculate effort (days)
#-------------------------------------------------------------------------------
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

# 0. Create final subset 
#############################################################################################

# remove tacsat with no fishing 
############################################
#
# remove tacsat with less or equal than 5 pings
############################################
id <- unique(tacsat$FT_REF[which(tacsat$Nping_trip>5)])
tacsat <- subset(tacsat, tacsat$FT_REF %in% id)


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
setwd(res.path)

# Link trips
###############################################################################################

uniquetacsat <- unique(tacsat[, c("VE_REF1", "SI_DATE", "FT_REF")])
maxTime<-   ddply(tacsat, .(FT_REF), summarise, maxTime=max(SI_TIME))
uniquetacsat$maxTime <- maxTime$maxTime[match(uniquetacsat$FT_REF, maxTime$FT_REF)]
uniquetacsat$id <- "tacsat"

uniqueeflalo <- unique(eflalo[, c("VE_REF1", "FT_LDAT", "FT_REF")])
names(uniqueeflalo)[names(uniqueeflalo)=="FT_LDAT"] <- "SI_DATE"
uniqueeflalo$id <- "eflalo"

uniquetacsat$FT_REF_eflalo<- NA
uniquetacsat$link0 <-NA
for(i in c(1,0, 2, 3, 4)){
  uniquetacsat$link0<-paste(uniquetacsat$VE_REF1,format(as.Date(uniquetacsat$SI_DATE,tz = "CET","%d/%m/%Y")+i,format = "%d/%m/%Y"),sep="_")
  uniquetacsat$FT_REF_eflalo[is.na(uniquetacsat$FT_REF_eflalo)] <- uniqueeflalo$FT_REF[match(uniquetacsat$link0[is.na(uniquetacsat$FT_REF_eflalo)], uniqueeflalo$FT_REF)]
}

uniquetacsat <- uniquetacsat[order(uniquetacsat$VE_REF1, as.Date(uniquetacsat$SI_DATE, format = "%d/%m/%Y")),]
uniquetacsat <- subset(uniquetacsat, select=- link0)

tacsat$FT_REF_0 <- tacsat$FT_REF  # FT_REF_0 is now the trip code for all tacsat trips
tacsat$FT_REF <- uniquetacsat$FT_REF_eflalo[match(tacsat$FT_REF_0, uniquetacsat$FT_REF)]  # FT_REF is now the code for linked trips
tacsat$FT_REF[is.na(tacsat$FT_REF)] <- 0

#Effort
eflalo <- data.frame(eflalo) # format each of the columns to the specified class

eflalo$INTV1           <- difftime(eflalo$FT_LDATIM,eflalo$FT_DDATIM,units="days")
eflalo$dummy          <- 1
res                   <- aggregate(eflalo$dummy,by=list(eflalo$FT_REF),FUN=sum,na.rm=T)
colnames(res)         <- c("FT_REF","nrRecord")
eflalo                <- merge(eflalo,res,by="FT_REF")
eflalo$INTV1           <- eflalo$INTV1 / eflalo$nrRecord
eflalo$LE_KG_DAS      <- eflalo$INTV1

  # 2. adding variables: gear and harbour
############################################

tacsat$LE_GEAR <- eflalo$LE_GEAR[match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$FT_LHAR <- eflalo$FT_LHAR[match(tacsat$FT_REF,eflalo$FT_REF)]

tacsat$VE_LEN <- eflalo$VE_LEN[match(tacsat$FT_REF,eflalo$FT_REF)]  
tacsat$LE_MSZ <- eflalo$LE_MSZ[match(tacsat$FT_REF,eflalo$FT_REF)]

tacsat$VE_KW     <- eflalo$VE_KW[  match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$LE_RECT   <- eflalo$LE_RECT[ match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$LE_MET    <- eflalo$LE_MET[  match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$VE_FLT    <- eflalo$VE_FLT[  match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$LE_CDAT   <- eflalo$LE_CDAT[ match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$VE_COU    <- eflalo$VE_COU[ match(tacsat$FT_REF,eflalo$FT_REF)]

table(tacsat$LE_GEAR)
table(tacsat$FT_LHAR)


## 3. Calculamos esfuerzo, y capturas para cada modalidad
##########################################################

tabla_resumen <- NULL
tabla_resumen_2 <- NULL

AllTacsatEflalo <- list()

#- Read in fixed speed bounds for different gears
# !!! Make sure that all your gears are added to this speedarr.csv list, some may be lacking !!! #

speedarr              <- read.csv(file=paste(dataPath,"/speedarr.csv",sep=""))
colnames(speedarr)    <- c("LE_GEAR","min","max")
speedarr$LE_GEAR      <- ac(speedarr$LE_GEAR)
speedarr              <- speedarr[speedarr$LE_GEAR%in%unique(tacsat$LE_GEAR),]
speedarr              <- speedarr[!duplicated(speedarr$LE_GEAR),]

tacsat <- tacsat[tacsat$LE_GEAR%in%speedarr$LE_GEAR,]

#-------------------------------------------------------------------------------
#- Assign the activity per gear
#-------------------------------------------------------------------------------
gearlist                 <- unique(tacsat$LE_GEAR)
tacsat$SI_STATE<-"0"
for (mm in gearlist) {
  tacsat$SI_STATE[tacsat$LE_GEAR==mm& tacsat$SI_SP >= an(speedarr[speedarr$LE_GEAR==mm,"min"])& tacsat$SI_SP <= an(speedarr[speedarr$LE_GEAR==mm,"max",])]<-"1"
}

id <- unique(tacsat$FT_REF[which(tacsat$SI_STATE=="1")])
tacsat <- subset(tacsat, tacsat$FT_REF %in% id)

  ##############################
  tacsatSubset <- subset(tacsat,!is.na(SI_SP) & INTV>0)
  tacsatSubset$SI_YEAR   <- year(as.POSIXct(tacsatSubset$SI_DATE, tz = "GMT", format = "%d/%m/%Y"))

  # Effort
  ##############################
  tacsatSubset$EFFORT <- tacsatSubset$INTV
  summary(tacsatSubset$EFFORT)
  
  tacsatSubset$SI_STATE <- as.numeric(tacsatSubset$SI_STATE)
  tacsatSubset               <- subset(tacsatSubset, SI_STATE==1)
  
  
  # Catches
  #####    
  
  # splitAmongPings: Crucial in this example below is that the elements 'kg in eflalo', 'kg in merged tacsat' 
  #                  and 'kg removed from eflalo' present you with identical values.
  #                  you might encounter some warnings or even errors, that is not a problem.
  #     'conserve'=TRUE   the Kg of the trips which cannot be linked are distributed among the linked trips. 
  #                       in this case, you would expect that tacsatEflalo would hold the same amount of KG and EURO as in the eflalo dataset.
  #     'conserve'=FALSE  the Kg of the trips which cannot be linked are NOT distributed among the linked trips. 
  #                       in this case, you would expect that tacsatEflalo would hold LESS KG and EURO than the eflalo dataset.
  #     'by'=INTV         the catches are proportional to the value of the interval (=effort)


  #- Split eflalo up in two sets, one set that cannot be merged to tacsat and one which matches
  eflaloNM              <- subset(eflalo,!FT_REF %in% unique(tacsat$FT_REF))
  eflaloM               <- subset(eflalo, FT_REF %in% unique(tacsat$FT_REF))
  
  #Disable the function in splitAmongPings that assigns 0 values to rectangles that is not in the eflalo
  tacsat$LE_RECT         <- "ALL"
  eflalo$LE_RECT         <- "ALL"

  eflaloM <- data.table(eflaloM)
  
  tacsatEflalo <- splitAmongPings(tacsat=data.frame(tacsatSubset),eflalo=data.frame(eflaloM),variable="all",level="trip",by="INTV",conserve=F)
  
  
  cat("total value in eflalo\n")
  print(colSums(eflaloM[c(grep("KG",colnames(eflaloM)))],na.rm=T))
  cat("total value in tacsatEflalo\n")
  print(colSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T))
  cat("ratio total value\n")
  print(colSums(eflaloM[c(grep("KG",colnames(eflaloM)))],na.rm=T)/ colSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T))
  
   # Create List
   #####   
   AllTacsatEflalo <- tacsatEflalo


lapply(AllTacsatEflalo, dim)

# Save results

setwd(dataPath)
save(tacsat, eflalo, AllTacsatEflalo, harbours, coastline_pol, file="LinkedTacsatEflalo.RData")

load("LinkedTacsatEflalo.RData")

###################      WRITE  TABLES       ###################
################################################################


## Result table csv
################################
setwd(res.path)

for (gear in gearlist){
write.table(AllTacsatEflalo[AllTacsatEflalo$LE_GEAR==gear,], paste("0_AISwithsales_", gear,"_trip.csv", sep=""), sep=",",row.names = FALSE)
}


# Trips linked by vessel
##############################################################################

linkedtrips <- unique(tacsat$FT_REF[tacsat$FT_REF > 0])

linktab_vessel_tacsat<-   ddply(tacsat, .(VE_REF), summarise, 
                                Ntrips_tacsat=length(unique(FT_REF_0)), 
                                Ntrips_tacsat_fishing=length(unique(FT_REF_0[SI_STATE=="f"])), 
                                Ntrips_tacsat_fishing_linked=length(unique(FT_REF[SI_STATE=="f" & FT_REF!=0])))
linktab_vessel_eflalo<-   ddply(eflalo, .(VE_REF), summarise, 
                                Ntrips_eflalo=length(unique(FT_REF)))

linktab_vessel <- merge(linktab_vessel_tacsat,linktab_vessel_eflalo, all=TRUE, by=c("VE_REF"))
linktab_vessel$Ntrips_eflalo[is.na(linktab_vessel$Ntrips_eflalo)]<- 0
linktab_vessel <- linktab_vessel[,c("VE_REF", "Ntrips_eflalo","Ntrips_tacsat", "Ntrips_tacsat_fishing", "Ntrips_tacsat_fishing_linked")]

write.table(linktab_vessel, "0_Linked_Trips_Vessel_summary.csv", row.names = FALSE, sep=",", dec=".")


# Trips linked by gear
##############################################################################

sum_gear_table <-NULL

test <- data.frame(AllTacsatEflalo)
test$LE_KG_TOT  <- colSums(test[,grep("LE_KG_",colnames(test))],na.rm=T)

AllTacsatEflalo$LE_KG_TOT  <- colSums(eflalo[,grep("LE_KG_",colnames(eflalo))],na.rm=T)
AllTacsatEflalo$LE_EURO_TOT<- colSums(eflalo[,grep("LE_EURO_",colnames(eflalo))],na.rm=T)

for (gear in gearlist){
sum_gear_table<- rbind(sum_gear_table,
                  data.frame(gear=gear,
                             trip_tot_eflalo=length(unique(eflalo$FT_REF[eflalo$LE_GEAR==gear])),
                             trip_tacsat_fishing=length(unique(tacsat$FT_REF_0[tacsat$LE_GEAR==gear])),
                             trip_linked=length(unique(AllTacsatEflalo$FT_REF[AllTacsatEflalo$LE_GEAR==gear]))
                            ))
}

write.table(sum_gear_table,paste("0_Linked_Trips_Gear_summary.csv", sep=""), sep=",",row.names = FALSE)


  
  
  # # Create a detailed table to explore why some trips were not linked
  # ############################################################################
  # link_id <- uniquetacsat$FT_REF_eflalo[which(!is.na(uniquetacsat$FT_REF_eflalo))]
  # eflalo_nolink <- uniqueeflalo[which(!uniqueeflalo$FT_REF %in% link_id),]
  # 
  # alltrips<- rbind.fill(uniquetacsat,eflalo_nolink)
  # alltrips <- alltrips[order(alltrips$VE_REF, as.Date(alltrips$SI_DATE, format = "%d/%m/%Y")),]
  # 
  # setwd(path.res)
  # write.table(alltrips, "Linked_Trips_detail.csv", row.names = FALSE, sep=",", dec=".")
  # 
  
  
###################       SOME PLOTS         ###################
################################################################

setwd(res.path)

 for (gear in gearlist){
  
  temp <- AllTacsatEflalo [[gear]] 
  
# Plot effort
require(RColorBrewer)
png(filename=paste(gear,"_effort_trip.png", sep=""), width=900, height = 900)
plotTools(temp,level="gridcell",xlim=c(-4,-1),ylim=c(43,44),zlim=NULL,log=F,gridcell=c(0.01,0.01),color=NULL,control.tacsat=list(clm="EFFORT"))
title(paste(gear,"_effort_trip", sep=""))
dev.off()


  
  # kg por especie
  for (i in c("LE_KG_COE" ,"LE_KG_HKE" ,"LE_KG_HOM", "LE_KG_MAC" ,"LE_KG_MUR")){
      #windows(8,8)
      png(filename=paste(gear,i,"trip.png", sep="_"), width=900, height = 900)
      plotTools(temp,level="gridcell",xlim=c(-4,-1),ylim=c(43,44),log=F,gridcell=c(0.01,0.01),
                color=NULL,control.tacsat=list(clm= c(i)))
      title(paste(gear,i, sep="_"))
      dev.off()
  }

}
  
  
  

  




