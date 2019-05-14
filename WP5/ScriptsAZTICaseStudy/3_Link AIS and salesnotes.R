
# R 3.4.4 
##### #


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


# Load data ----
########################################### #
setwd(path)

load("Cleantacsat.RData")
load("Cleaneflalo.RData")
#load("eurPols.RData")


# 0. Create final subset ----
########################################### #

# remove tacsat with no fishing 
########################################### #
id <- unique(tacsat$FT_REF[which(tacsat$SI_STATE=="f")])
tacsat <- subset(tacsat, tacsat$FT_REF %in% id)

# remove tacsat with less than or equal 5 pings fishing
########################################### #
id <- unique(tacsat$FT_REF[which(tacsat$Nping_trip_fish>5)])
tacsat <- subset(tacsat, tacsat$FT_REF %in% id)

# clean gear
########################################### #
unique(eflalo$LE_GEAR)
eflalo$LE_GEAR[eflalo$LE_GEAR %in% c("GNS", "GTR")] <- "GXX"


# 1. merge vms and logbook ----
############################################################### #
#  I have used a loop instead of the function mergeEflalo2Tacsat, to have more control on the link
#  Algorithm:
#            date tacsat + 1                =   date eflalo
#            date tacsat[not matched]       =   date eflalo
#            date tacsat[not matched] + 2   =   date eflalo
#            date tacsat[not matched] + 3   =   date eflalo
#            date tacsat[not matched] + 4   =   date eflalo
############################################################### #
setwd(path.res)

# Link trips
############################################################### #

uniquetacsat <- unique(tacsat[, c("VE_REF", "SI_DATE", "FT_REF")])
maxTime<-   ddply(tacsat, .(FT_REF), summarise, maxTime=max(SI_TIME))
uniquetacsat$maxTime <- maxTime$maxTime[match(uniquetacsat$FT_REF, maxTime$FT_REF)]
uniquetacsat$id <- "tacsat"

uniqueeflalo <- unique(eflalo[, c("VE_REF", "FT_LDAT", "FT_REF", "LE_GEAR")])
names(uniqueeflalo)[names(uniqueeflalo)=="FT_LDAT"] <- "SI_DATE"
uniqueeflalo$id <- "eflalo"

uniquetacsat$FT_REF_eflalo<- NA
uniquetacsat$link0 <-NA
for(i in c(1,0,2, 3,4)){
  uniquetacsat$link0<-paste(uniquetacsat$VE_REF,format(as.Date(uniquetacsat$SI_DATE,tz = "CET","%d/%m/%Y")+i,format = "%d/%m/%Y"),sep="_")
  uniquetacsat$FT_REF_eflalo[is.na(uniquetacsat$FT_REF_eflalo)] <- uniqueeflalo$FT_REF[match(uniquetacsat$link0[is.na(uniquetacsat$FT_REF_eflalo)], uniqueeflalo$FT_REF)]
}

uniquetacsat <- uniquetacsat[order(uniquetacsat$VE_REF, as.Date(uniquetacsat$SI_DATE, format = "%d/%m/%Y")),]
uniquetacsat <- subset(uniquetacsat, select=- link0)
uniquetacsat$LE_GEAR <- uniqueeflalo$LE_GEAR[match(uniquetacsat$FT_REF_eflalo, uniqueeflalo$FT_REF)]  # FT_REF is now the code for linked trips


tacsat$FT_REF_0 <- tacsat$FT_REF  # FT_REF_0 is now the trip code for all tacsat trips
tacsat$FT_REF <- uniquetacsat$FT_REF_eflalo[match(tacsat$FT_REF_0, uniquetacsat$FT_REF)]  # FT_REF is now the code for linked trips
tacsat$FT_REF[is.na(tacsat$FT_REF)] <- 0


  # 2. adding variables: gear and harbour ----
########################################### #

tacsat$LE_GEAR <- eflalo$LE_GEAR[match(tacsat$FT_REF,eflalo$FT_REF)]
tacsat$FT_LHAR <- eflalo$FT_LHAR[match(tacsat$FT_REF,eflalo$FT_REF)]

table(tacsat$LE_GEAR)
table(tacsat$FT_LHAR)

# Read csv with all tacsat trips assigned to a gear



## 3. Split catches among pings ---- 
#     This code allows:
#         a) splitting catches among trips (just uses linked trips) 
#         b) splitting catches of the combination vessel+month+gear among corresponding pings
#         c) splitting catches of the combination vessel+month+gear and split the remaining catch proprcionally among the linked pins
#         
#
######################################################### #



 # . link trip (vessel day) ----
 ######################## #
  tacsat_ef<- tacsat %>% mutate(effishing=ifelse(SI_STATE=="f",INTV,0))%>%
          group_by(FT_REF) %>%
          mutate(sumf=sum(effishing),propef=effishing/sumf)%>%
          ungroup() %>%
          mutate(month=months(as.Date(SI_DATE,tz = "CET","%d/%m/%Y")),  # proportions for a) splitting among trips
                 FT_REF_MONTH=paste(VE_REF, month,LE_GEAR, sep="_"))  %>%
          group_by(FT_REF_MONTH) %>%
          mutate(sumfm=sum(effishing),propefm=effishing/sumfm)%>%       # proportions for b) splitting among vessel+month+gear
          ungroup()

  simpw<-eflalo[,c(7,23:128)]%>%
                group_by(FT_REF)%>%
                summarise_all(sum,na.rm=T)%>%
                ungroup()

  tacsatEflalo <- tacsat_ef %>%  left_join(simpw,by=c("FT_REF"="FT_REF")) %>%
    mutate_at(.funs = funs(. * propef), .vars = vars(contains("LE_KG")))  

  # Create list
  AllTacsatEflalo <- list()
  gearlist <- unique(tacsat$LE_GEAR[!is.na(tacsat$LE_GEAR)])
  for (gear in gearlist){ AllTacsatEflalo[[which(gearlist==gear)]] <- subset(tacsatEflalo,LE_GEAR==gear)}
  names (AllTacsatEflalo) <- gearlist

  
  
  # . link vessel month ----
  ######################## #
  eflalo<- eflalo %>% mutate(month=months(as.Date(FT_DDAT,tz = "CET","%d/%m/%Y")),
                             FT_REF_MONTH=paste(VE_REF, month, LE_GEAR, sep="_"))  
  
  simpw2<- eflalo %>% subset(select=c("FT_REF_MONTH", colnames(eflalo)[grep("LE_KG", colnames(eflalo))]))  %>%
                               group_by(FT_REF_MONTH)  %>%
                               summarise_all(sum,na.rm=T)  %>%
                               ungroup()
  
  tacsatEflalo_month <- tacsatEflalo %>% subset(!is.na(LE_GEAR))  %>%
    subset(select=c( colnames(tacsatEflalo)[-grep("LE_KG", colnames(tacsatEflalo))]))  %>%
    left_join(simpw2,by=c("FT_REF_MONTH"="FT_REF_MONTH")) %>% 
    mutate_at(.funs = funs(. * propefm), .vars = vars(contains("LE_KG")))
  
  # Create list
  AllTacsatEflalo_month <- list()
  gearlist <- unique(tacsat$LE_GEAR[!is.na(tacsat$LE_GEAR)])
  for (gear in gearlist){ AllTacsatEflalo_month[[which(gearlist==gear)]] <- subset(tacsatEflalo_month,LE_GEAR==gear)}
  names (AllTacsatEflalo_month) <- gearlist

  
  # . Conserve TRUE - by bymonth ----
  ######################## #
  temp<- tacsatEflalo_month
  
  tacsatEflalo_prop<- temp  %>% 
   group_by(LE_GEAR) %>%
    mutate_at(.vars = vars(contains("LE_KG")), .funs = funs(. / sum(., na.rm = TRUE)))  %>%
    replace(., is.na(.), 0) %>%
    ungroup()
  
  simpw3<- eflalo %>% subset(select=c( "LE_GEAR", colnames(eflalo)[grep("LE_KG", colnames(eflalo))]))  %>%
    group_by(LE_GEAR) %>%
    summarise_all(sum,na.rm=T)  %>%
    ungroup()
  
  AllTacsatEflalo_conserve <- list()
  gearlist <- unique(tacsat$LE_GEAR[!is.na(tacsat$LE_GEAR)])
  
  for (gear in gearlist){
    t0 <- subset(tacsatEflalo_prop, LE_GEAR==gear, select=c(colnames(tacsatEflalo_prop)[-grep("LE_KG", colnames(tacsatEflalo_prop))]))
    t1<- as.data.frame(subset(tacsatEflalo_prop, LE_GEAR==gear, select=c( colnames(tacsatEflalo_prop)[grep("LE_KG", colnames(tacsatEflalo_prop))])))
    t2<- as.data.frame(subset(simpw3, LE_GEAR==gear, select=c( colnames(simpw3)[grep("LE_KG", colnames(simpw3))])))
    t12 <- as.data.frame( mapply(`*`, t1, as.vector(t2)))
        tfin <- cbind(t0, t12)
        AllTacsatEflalo_conserve[[which(gearlist==gear)]] <- tfin
    }
  
  names (AllTacsatEflalo_conserve) <- gearlist
  lapply(AllTacsatEflalo_conserve, dim)
  

  
# Some chekings ---- 
  tapply(eflalo$LE_KG_HKE, eflalo$LE_GEAR, sum)
  tapply(eflalo$LE_KG_HKE[eflalo$FT_REF %in% unique(tacsat_ef$FT_REF)], eflalo$LE_GEAR[eflalo$FT_REF %in% unique(tacsat_ef$FT_REF)], sum)
  sum(AllTacsatEflalo[["GXX"]]$LE_KG_HKE, na.rm=TRUE)
  sum(AllTacsatEflalo_month[["GXX"]]$LE_KG_HKE, na.rm=TRUE)
  sum(AllTacsatEflalo_conserve[["GXX"]]$LE_KG_HKE, na.rm=TRUE)

  lapply(AllTacsatEflalo, dim)
  lapply(AllTacsatEflalo_month, dim)
  lapply(AllTacsatEflalo_conserve, dim)
  
  
# Save results ---- 
setwd(path)
save(tacsat, tacsat_ef, eflalo, AllTacsatEflalo, AllTacsatEflalo_month, AllTacsatEflalo_conserve, harbours, coastline_pol, 
     file="LinkedTacsatEflalo.RData")



# # Create a detailed table to explore why some trips were not linked ----
# ########################################################################### #
link_id <- uniquetacsat$FT_REF_eflalo[which(!is.na(uniquetacsat$FT_REF_eflalo))]
eflalo_nolink <- uniqueeflalo[which(!uniqueeflalo$FT_REF %in% link_id),]

alltrips<- rbind.fill(uniquetacsat,eflalo_nolink)
alltrips <- alltrips[order(alltrips$VE_REF, as.Date(alltrips$SI_DATE, format = "%d/%m/%Y")),]

setwd(path.res)
write.table(alltrips, "0_Linked_Trips_detail.csv", row.names = FALSE, sep=",", dec=".")



