####################################
rm(list=ls())
library(vmstools)
library(Hmisc)
library(lubridate)
library(sp)
library(maps);library(mapdata)
library(haven)
library(data.table)

year <- 2017

# -- load data   -- #
sysPa       <-"C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\"

dataPath  <-    paste(sysPa,"Rdata\\",sep="")   
inputpath<- "C:\\joeg\\180924_FishPi_WP5_SSF\\R_workflow\\"

# take only the vessels that we have on tacsat file  
eflalo <- fread(paste(inputpath,"eflalo5_", year,".csv",sep=''))
eflalo <- data.frame(eflalo)
eflalo2 <- formatEflalo(eflalo) # format each of the columns to the specified class
eflalo <- data.table(eflalo2)


############## - Cleaning logbook data - ################
#########################################################

# 1. remove duplicate records 

#In principle, the LE_ID identifier should be unique, this allows for an easy solution
#Checking for duplicates  

head(eflalo)
dim(eflalo[duplicated(eflalo$LE_ID),])

#1. Wrong date: landing date happens before departure
#Create a date-time stamp for the departure date
eflalo$FT_DDATIM <- as.POSIXct(paste(eflalo$FT_DDAT,  eflalo$FT_DTIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M") #turn date and time into one date-time stamp

#Create a date-time stamp for the landing date
eflalo$FT_LDATIM <- as.POSIXct(paste(eflalo$FT_LDAT,  eflalo$FT_LTIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M") #turn date and time into one date-time stamp

eflalo$LE_CDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                                tz = "Europe/Paris", format = "%d/%m/%Y  %H:%M")

#Now see where the landing date happens before departure
idx               <- which(eflalo$FT_LDATIM > eflalo$FT_DDATIM)
print(nrow(eflalo))

#only keep the records we want
eflalo            <- eflalo[idx,]
print(nrow(eflalo))

#Redefine FT_REF to match with TACSAT
eflalo$FT_REF <- paste(eflalo$VE_REF1, format(as.Date(eflalo$FT_LDAT, format = "%d/%m/%Y")+1, format="%d/%m/%Y"), sep="_")

############## - Save data - ################
############################################
save(eflalo,file=file.path(dataPath, paste0('eflaloClean_', year, '.RData')))













