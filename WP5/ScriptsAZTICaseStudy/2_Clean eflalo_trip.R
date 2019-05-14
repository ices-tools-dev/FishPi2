
# -- vmstools ----
################################### #
rm(list=ls())
library(vmstools)
library(Hmisc)
library(lubridate)
library(sp)
library(maps);library(mapdata)



# -- load data   ----
################################### # Esta parte la necesito?? preguntar a Lucia

path<- "C:\\use\\0_Lucia\\1_Proyectos\\IM17_Bategin II\\AIS 2017\\Data"
setwd(path)
load("Datos_art_2017_trip.RData")


# changethe name of the file  

eflalo <- eflalo_art



############## - Cleaning eflalo data ----
######################################################## #



# 1. remove duplicate records 

#In principle, the LE_ID identifier should be unique, this allows for an easy solution
#Checking for duplicates  

eflalo <- eflalo

head(eflalo)

dim(eflalo[duplicated(eflalo$LE_ID),])
#eflalo <- eflalo[!duplicated(eflalo$LE_ID),]



#1. Wrong date: landing date happens before departure

eflalop           <- eflalo

#Create a date-time stamp for the departure date
eflalop$FT_DDATIM <- as.POSIXct(paste(eflalo$FT_DDAT,  eflalo$FT_DTIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M") #turn date and time into one date-time stamp

#Create a date-time stamp for the landing date
eflalop$FT_LDATIM <- as.POSIXct(paste(eflalo$FT_LDAT,  eflalo$FT_LTIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M") #turn date and time into one date-time stamp

#Now see where the landing date happens before departure
idx               <- which(eflalop$FT_LDATIM >= eflalop$FT_DDATIM)
print(nrow(eflalo))

#only keep the records we want
eflalo            <- eflalo[idx,]
print(nrow(eflalo))





############## - Save data ----
########################################### #


save(eflalo,file="Cleaneflalo.RData")














