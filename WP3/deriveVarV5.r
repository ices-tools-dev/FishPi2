deriveVar <-function(obj)
{
#-----------------------------------------------
# function to derive variables from standard fishPi2 data sets
# uses the ASFIS_WoRMS, UNLOCODE data sets. 
# to get a sub UK landCtry variable the portCodeUK data set is used
# v2 29 Mar 2018  format.Date used in preference to cut to derive $week
# AP, NP (and fiahpi2 WP2 WP3 team)
#----------------------------------------------------

library(fishPiCodes)
data(ASFIS_WoRMS)
data(UNLOCODE)
data(portCodeUK)
data(rectangles)
#-------------------------------------------------------------
# summary functions
lengthUnique <-function(x){length(unique(x))}

nameMax <-function(x){names(which.max(table(x)))}



#-------------------------------------------------------------

# some additional date and time variables 
obj$depDates <-as.Date(obj$depDate)
obj$landDates <-as.Date(obj$landDate)
obj$daysAtSea <-difftime(obj$landDates,obj$depDates,units="days")
obj$depYear <-as.POSIXlt(obj$depDates)$year+1900
obj$landYear <-as.POSIXlt(obj$landDates)$year+1900

obj$depYday <-(as.POSIXlt(obj$depDates)$yday)
obj$landYday <-(as.POSIXlt(obj$landDates)$yday)

# obj$week <-cut(obj$yday,breaks=seq(0,364,7),labels=seq(1:52))
# above generates NA for yday==0
obj$depWeek<-as.numeric(format(obj$depDates, format="%W"))
obj$depMonth <-months(obj$depDates,abb=T)
obj$landWeek<-as.numeric(format(obj$landDates, format="%W"))
obj$landMonth <-months(obj$landDates,abb=T)

# this gives 53 weeks 00 to 52 

obj$depDay <-weekdays(as.POSIXlt(obj$depDates), abbreviate=T)
obj$landDay <-weekdays(as.POSIXlt(obj$landDates), abbreviate=T)

# generating FAO codes and ISSCAAP groupings for the species
index <-match(obj$sppCode,ASFIS_WoRMS$AphiaID)
obj$sppFAO <-as.character(ASFIS_WoRMS$X3A_CODE[index])
obj$ISSCAAP <-ASFIS_WoRMS$ISSCAAP[index]
obj$sppEng <-ASFIS_WoRMS$English_name[index]

# use the AphiaID_accepted column to get the FAO match for any spp 
# which map to unaccepted AphiaID values  
if(any(is.na(obj$sppFAO)))
{
index2 <-which(is.na(obj$sppFAO))
index3 <-match(obj$sppCode[index2],ASFIS_WoRMS$AphiaID_accepted)
obj$sppFAO[index2] <-as.character(ASFIS_WoRMS$X3A_CODE[index3])
obj$ISSCAAP[index2] <-ASFIS_WoRMS$ISSCAAP[index3]
obj$sppEng[index2] <-ASFIS_WoRMS$English_name[index3]
}



obj$ssLon <-rectangles$lon[match(obj$rect,rectangles$statsq)]
obj$ssLat <-rectangles$lat[match(obj$rect,rectangles$statsq)]



# generating the landCtry and logitude and latitude of the landing location
index2 <-match(obj$landLoc,UNLOCODE$loCode)
obj$landCtry <-UNLOCODE$ctryCode[index2]
obj$landName <-UNLOCODE$locName[index2]
obj$landLon <-UNLOCODE$lon[index2]
obj$landLat <-UNLOCODE$lat[index2]

# to get sub UK level landCtry we use the portCodeUK data set to get 
# Isle of Man, Channel Islands, Wales, Scotland, Northern Ireland and England
if(any(obj$landCtry %in% c("GBR","IMN","GGY","JEY")))
{
indexGB <-which(obj$landCtry %in% c("GBR","IMN","GGY","JEY"))
index3 <-match(obj$landLoc[indexGB],portCodeUK$loCode)
obj$landCtry[indexGB] <-portCodeUK$ctryCode[index3]
}

# getting the DCF gear code from the foCatEu6
x <-strsplit(obj$foCatEu6, "_",fixed=T)
x2 <-matrix(unlist(x),length(x),length(x[[1]]),byrow=T)
obj$gear <-x2[,1]
obj$target <-x2[,2]
obj$meshRange <-x2[,3]

# variables related to the landings of the trip 
# added 27/4/2018 AP

# landed weight of the trip
tripLandWt <-tapply(obj$landWt,obj$fishTripId,sum)
obj$tripLandWt <-tripLandWt[match(obj$fishTripId,names(tripLandWt))]


# number of species for the trip
tripNumSpp <-tapply(obj$sppCode,obj$fishTripId,lengthUnique)
obj$tripNumSpp <-tripNumSpp[match(obj$fishTripId,names(tripNumSpp))]


# trip max Weight and max weight spp
tripSppXWt <-tapply(obj$landWt,list(obj$sppFAO,obj$fishTripId),sum)
tripMaxSppWt <-apply(tripSppXWt,2,max,na.rm=T)
tripMaxSpp <-row.names(tripSppXWt)[apply(tripSppXWt,2,which.max)]

obj$tripMaxSppWt <-tripMaxSppWt[match(obj$fishTripId,names(tripMaxSppWt))]
obj$tripMaxSpp <-tripMaxSpp[match(obj$fishTripId,names(tripMaxSppWt))]

# max species wgt as proportion of landing wgt 
obj$propMaxSpp <-obj$tripMaxSppWt/obj$tripLandWt

# aggregating stock information
data(stocks)

stocks$stock<- as.character(stocks$stock)
stocks$sppFAO<- as.character(stocks$sppFAO)
#stocks$desc<- as.character(stocks$desc)
#stocks$areaType<- as.character(stocks$areaType)

aggArea<- c("27.1" ,"27.2","27.2.a"  ,"27.3", "27.3.a","27.3.b","27.3.c","27.3.d", "27.4","27.4.a","27.4.b","27.5","27.5.a","27.5.b",
            "27.6","27.6.a","27.6.b", "27.7","27.7.a","27.7.c","27.7.j","27.7.k","27.8","27.8.c","27.8.d","27.8.e","27.9","27.9.a","27.9.b",
            "27.10","27.10.a","27.11","27.12","27.12.a","27.13","27.14","27.14.b", "27", "21", "21.1", "21.2", "21.3", "21.4")

for (i in aggArea){
  temp<-stocks[stocks$area==i,]
  temparea<- unique(stocks$Areas[substr(stocks$area,1,nchar(i)+1)==paste(i,".", sep="")])
  
  copytemp <- as.data.frame(lapply(temp, rep, length(temparea)))    
  copytemp$area <- unlist(lapply(temparea, rep, dim(temp)[1])) 
  
  stocks <- rbind(stocks, copytemp)
}
stocks$stockid <- paste(stocks$sppFAO, stocks$area, sep="_")

obj_stockid <- paste(obj$sppFAO, obj$area, sep="_")
obj$stock <- as.character(stocks$stock[match(obj_stockid, stocks$stockid)])
obj$stock[is.na(obj$stock)] <- "no.defined.stock"

return(obj)
}
