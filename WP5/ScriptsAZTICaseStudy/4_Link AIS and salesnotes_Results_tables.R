
# R 3.4.4 
###


rm(list=ls())
library(dplyr)
library(plyr)

# Set paths ----
########################################### #
path<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Data"
path.res<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Results\\_trip"


# Load results -----
setwd(path)

load("LinkedTacsatEflalo.RData")

###################      WRITE  TABLES       ############### ####
############################################################ #
setwd(path.res)
gearlist <- unique(tacsat$LE_GEAR[!is.na(tacsat$LE_GEAR)])


## Result table csv ----
################################

for (gear in gearlist){
write.table(AllTacsatEflalo [[gear]], paste("0_AISconCapturas_", gear,"_trip.csv", sep=""), sep=",",row.names = FALSE)
write.table(AllTacsatEflalo_month [[gear]], paste("0_AISconCapturas_", gear,"_month.csv", sep=""), sep=",",row.names = FALSE)
write.table(AllTacsatEflalo_conserve [[gear]], paste("0_AISconCapturas_", gear,"_conserve.csv", sep=""), sep=",",row.names = FALSE)
}



# Summary tables about the performance of the link eflalo + tacsat ----
############################################################################# #

# . Trips linked by vessel ----
############################################################################# #

linkedtrips <- unique(tacsat$FT_REF[tacsat$FT_REF > 0])

linktab_vessel_tacsat<-   ddply(tacsat_ef, .(VE_REF), summarise, 
                                Ntrips_tacsat_fishing=length(unique(FT_REF_0[SI_STATE=="f"])), 
                                Ntrips_tacsat_fishing_linked=length(unique(FT_REF[SI_STATE=="f" & FT_REF!=0])),
                                Nmonth_tacsat_fishing_linked=length(unique(FT_REF_MONTH[SI_STATE=="f" & FT_REF!=0])))
linktab_vessel_eflalo<-   ddply(eflalo, .(VE_REF), summarise, 
                                Ntrips_eflalo=length(unique(FT_REF)),
                                Nmonth_eflalo=length(unique(FT_REF_MONTH)))

linktab_vessel <- merge(linktab_vessel_tacsat,linktab_vessel_eflalo, all=TRUE, by=c("VE_REF"))
linktab_vessel$Ntrips_eflalo[is.na(linktab_vessel$Ntrips_eflalo)]<- 0
linktab_vessel$Nmonth_eflalo[is.na(linktab_vessel$Nmonth_eflalo)]<- 0
linktab_vessel <- linktab_vessel[,c("VE_REF", "Ntrips_eflalo", "Ntrips_tacsat_fishing", "Ntrips_tacsat_fishing_linked",
                                    "Nmonth_eflalo","Nmonth_tacsat_fishing_linked")]

write.table(linktab_vessel, "0_Linked_TripsMonths_Vessel_summary.csv", row.names = FALSE, sep=",", dec=".")


# . Trips linked by gear - Trips ----
############################################################################# #

sum_gear_table <-NULL

for (gear in gearlist){
  
temp <-  data.frame(AllTacsatEflalo [[gear]])
sum_gear_table<- rbind(sum_gear_table,
                  data.frame(gear=gear,
                             trip_tot_eflalo=length(unique(eflalo$FT_REF[eflalo$LE_GEAR==gear])),
                             trip_tacsat_fishing=length(unique(tacsat_ef$FT_REF_0[tacsat_ef$LE_GEAR==gear])),
                             trip_linked=length(unique(temp$FT_REF)),
                             kg_tot=sum(colSums(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_COE", "LE_KG_HKE", "LE_KG_HOM", "LE_KG_MAC", "LE_KG_MUR","LE_KG_ALB")], na.rm=TRUE) ) ,
                             kg_linked=sum(colSums(temp[,c("LE_KG_COE", "LE_KG_HKE", "LE_KG_HOM", "LE_KG_MAC", "LE_KG_MUR","LE_KG_ALB")], na.rm=TRUE) ),
                             kg_tot_COE=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_COE")], na.rm=TRUE) ,
                             kg_linked_COE=sum(temp[,c("LE_KG_COE")], na.rm=TRUE),
                             kg_tot_HKE=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_HKE")], na.rm=TRUE) ,
                             kg_linked_HKE=sum(temp[,c("LE_KG_HKE")], na.rm=TRUE),
                             kg_tot_HOM=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_HOM")], na.rm=TRUE) ,
                             kg_linked_HOM=sum(temp[,c("LE_KG_HOM")], na.rm=TRUE),
                             kg_tot_MAC=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_MAC")], na.rm=TRUE) ,
                             kg_linked_MAC=sum(temp[,c("LE_KG_MAC")], na.rm=TRUE),
                             kg_tot_MUR=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_MUR")], na.rm=TRUE) ,
                             kg_linked_MUR=sum(temp[,c("LE_KG_MUR")], na.rm=TRUE),
                             kg_tot_ALB=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_ALB")], na.rm=TRUE) ,
                             kg_linked_ALB=sum(temp[,c("LE_KG_ALB")], na.rm=TRUE)
                            ))
}

write.table(sum_gear_table,paste("0_Linked_Gear_summary_trip.csv", sep=""), sep=",",row.names = FALSE)



# . Trips linked by gear - Month ----
############################################################################# #

sum_gear_table <-NULL

for (gear in gearlist){
  
  temp <-  data.frame(AllTacsatEflalo_month [[gear]])
  sum_gear_table<- rbind(sum_gear_table,
                         data.frame(gear=gear,
                                    trip_tot_eflalo=length(unique(eflalo$FT_REF_MONTH[eflalo$LE_GEAR==gear])),
                                    trip_tacsat_fishing=length(unique(tacsat_ef$FT_REF_MONTH[tacsat_ef$LE_GEAR==gear])),
                                    trip_linked=length(unique(temp$FT_REF_MONTH)),
                                    kg_tot=sum(colSums(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_COE", "LE_KG_HKE", "LE_KG_HOM", "LE_KG_MAC", "LE_KG_MUR","LE_KG_ALB")], na.rm=TRUE) ) ,
                                    kg_linked=sum(colSums(temp[,c("LE_KG_COE", "LE_KG_HKE", "LE_KG_HOM", "LE_KG_MAC", "LE_KG_MUR","LE_KG_ALB")], na.rm=TRUE) ),
                                    kg_tot_COE=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_COE")], na.rm=TRUE) ,
                                    kg_linked_COE=sum(temp[,c("LE_KG_COE")], na.rm=TRUE),
                                    kg_tot_HKE=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_HKE")], na.rm=TRUE) ,
                                    kg_linked_HKE=sum(temp[,c("LE_KG_HKE")], na.rm=TRUE),
                                    kg_tot_HOM=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_HOM")], na.rm=TRUE) ,
                                    kg_linked_HOM=sum(temp[,c("LE_KG_HOM")], na.rm=TRUE),
                                    kg_tot_MAC=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_MAC")], na.rm=TRUE) ,
                                    kg_linked_MAC=sum(temp[,c("LE_KG_MAC")], na.rm=TRUE),
                                    kg_tot_MUR=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_MUR")], na.rm=TRUE) ,
                                    kg_linked_MUR=sum(temp[,c("LE_KG_MUR")], na.rm=TRUE),
                                    kg_tot_ALB=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_ALB")], na.rm=TRUE) ,
                                    kg_linked_ALB=sum(temp[,c("LE_KG_ALB")], na.rm=TRUE)
                         ))
}

write.table(sum_gear_table,paste("0_Linked_Gear_summary_month.csv", sep=""), sep=",",row.names = FALSE)



# . Trips linked by gear - Conserve ----
############################################################################# #

sum_gear_table <-NULL

for (gear in gearlist){
  
  temp <-  data.frame(AllTacsatEflalo_conserve [[gear]])
  sum_gear_table<- rbind(sum_gear_table,
                         data.frame(gear=gear,
                                    trip_tot_eflalo=length(unique(eflalo$FT_REF_MONTH[eflalo$LE_GEAR==gear])),
                                    trip_tacsat_fishing=length(unique(tacsat_ef$FT_REF_MONTH[tacsat_ef$LE_GEAR==gear])),
                                    trip_linked=length(unique(temp$FT_REF_MONTH)),
                                    kg_tot=sum(colSums(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_COE", "LE_KG_HKE", "LE_KG_HOM", "LE_KG_MAC", "LE_KG_MUR","LE_KG_ALB")], na.rm=TRUE) ) ,
                                    kg_linked=sum(colSums(temp[,c("LE_KG_COE", "LE_KG_HKE", "LE_KG_HOM", "LE_KG_MAC", "LE_KG_MUR","LE_KG_ALB")], na.rm=TRUE) ),
                                    kg_tot_COE=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_COE")], na.rm=TRUE) ,
                                    kg_linked_COE=sum(temp[,c("LE_KG_COE")], na.rm=TRUE),
                                    kg_tot_HKE=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_HKE")], na.rm=TRUE) ,
                                    kg_linked_HKE=sum(temp[,c("LE_KG_HKE")], na.rm=TRUE),
                                    kg_tot_HOM=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_HOM")], na.rm=TRUE) ,
                                    kg_linked_HOM=sum(temp[,c("LE_KG_HOM")], na.rm=TRUE),
                                    kg_tot_MAC=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_MAC")], na.rm=TRUE) ,
                                    kg_linked_MAC=sum(temp[,c("LE_KG_MAC")], na.rm=TRUE),
                                    kg_tot_MUR=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_MUR")], na.rm=TRUE) ,
                                    kg_linked_MUR=sum(temp[,c("LE_KG_MUR")], na.rm=TRUE),
                                    kg_tot_ALB=sum(eflalo[eflalo$LE_GEAR==gear,c("LE_KG_ALB")], na.rm=TRUE) ,
                                    kg_linked_ALB=sum(temp[,c("LE_KG_ALB")], na.rm=TRUE)
                         ))
}

write.table(sum_gear_table,paste("0_Linked_Gear_summary_conserve.csv", sep=""), sep=",",row.names = FALSE)

