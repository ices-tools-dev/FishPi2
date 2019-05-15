# This code  was used to create the 2015 North sea dataset for the North Sea case study


fishOfInterest <- c("PLE","POK","COD","HAD","SOL","WHG","HKE","ANF", "TUR","GUU","DAB","MUR","LEM","LIN","BLL","FLE","GUG","POL","WIT")
ctryOfInterest <-c("BEL","DEU","DNK","FRA","GBE","GBS","NLD","SWE")
widerNSea <- c("27.7.d" , "27.4.a" ,"27.4.b", "27.4.c" , "27.3.a.20" , "27.3.a.21", "27.3.a")
vars <-  c("vslFlgCtry","vslLenCls","fishTripId","landLoc","rect","area","foCatEu6","sppCode","sppName",   
           "landWt","landDay","landDate","sppFAO","ISSCAAP","landCtry","gear","stock")

lengthUnique <-function(x){length(unique(x))}

library(fishPiCodes)
data(ASFIS_WoRMS)
library(RCMfunctions)


path.data <-"X:/fishPi2/logbook data/"  # ENTER YOUR OWN PATH!
setwd(path.data)

# The files used in this are large, so this code uses processes small chunks (per country) and then joins them togther.
# This code also uses a reduced number of variables (identified in vars), to reduce file size. The resulting files have a 'sm' suffix  
#  The reduction in variables was only done to circumvent problems with processing power and can be removed.


#-----------------------------------------
# 2015
#-----------------------------------------
# R is struggling to work with large files, so we will save each part as a smaller file and try again
#load("2015_DeriveData.RData")  # has no FRA data

# There is no FRA datain the large derive data set, so it has to be added separately
#FRA_derived_2015 <- readRDS("X:/fishPi2/logbook data/FRA_derived_2015.rds") # get the 2015 FRA data


new.path.data <-"X:/fishPi2/logbook data/JESS indiv ctry working data files/"
setwd(new.path.data)

#FRA15sm <- FRA_derived_2015[,vars]
#save(FRA15sm, file = "FRA15sm.RData")
#BEL15sm <- BEL15[,vars]
#save(BEL15sm, file = "BEL15sm.RData")
#DEU15sm <- DEU15[,vars]
#save(DEU15sm, file = "DEU15sm.RData")
#DNK15sm <- DNK15[,vars]
#save(DNK15sm, file = "DNK15sm.RData")
#ESP15sm <- ESP15[,vars]
#save(ESP15sm, file = "ESP15sm.RData")
#IRL15sm <- IRL15[,vars]
#save(IRL15sm, file = "IRL15sm.RData")
#NLD15sm <- NLD15[,vars]
#save(NLD15sm, file = "NLD15sm.RData")
#SWE15sm <- SWE15[,vars]
#save(SWE15sm, file = "SWE15sm.RData")
#PRT15sm <- PRT15[,vars]
#save(PRT15sm, file = "PRT15sm.RData")
#UK15sm <- UK15[,vars]
#save(UK15sm, file = "UK15sm.RData")

# These files have a reduced number of colmns(17), so that it is easier to work with...

# France
load("FRA15sm.RData")
xx <- FRA15sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <-obj[demNorthSeaIndex,]
rm(xx,obj,FRA15sm)

# Belgium
load("BEL15sm.RData")
xx <- BEL15sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,BEL15sm)


# Germany
load("DEU15sm.RData")
xx <- DEU15sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,DEU15sm)

# Denmark
load("DNK15sm.RData")
xx <- DNK15sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,DNK15sm)

# Spain
# load("ESP15sm.RData")    # NO TRIPS IN THE N SEA AREA
# xx <- ESP15sm
# dolphinIndex <-which(xx$sppCode %in% "254960")
# xx[xx$sppCode == "254960",]
# if(length(dolphinIndex)>0)
# {
#   xx <-xx[-dolphinIndex,]
#   cat("Amazon River Dolphin removed from the North Sea \n")
# }
# 
# xx$landCtryFlag <-xx$landCtry
# xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)
# 
# xx$landType <-"own"
# xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
# xx$landTonnage <-xx$landWt/1000
# xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")
# 
# 
# # North Sea data set
# # this comprises all trips that have any landing that are recorded from the wider North Sea areas.  
# 
# NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
# NSareaIndex <-which(xx$area %in% NSarea)
# NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
# NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
# obj <-xx[NStripIndex ,] 
# 
# 
# # correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
# #obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
# obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37
# 
# 
# # making a sppType field based on groupings of the ISSCAAP codes
# obj$sppType <-"other"
# 
# obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
# obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
# obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
# obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
# obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
# obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
# obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
# obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
# obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
# obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
# obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"
# 
# 
# 
# # giving each trip a "main" sppType based on the landed weight by sppType code 
# tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
# tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
# tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
# tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
# obj$tripMainSppType <-tripMaxSppTypeChecked
# 
# demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
# demNS <- rbind(demNS, obj[demNorthSeaIndex,])
# rm(xx,obj,ESP15sm)

# Ireland
load("IRL15sm.RData")
xx <- IRL15sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,IRL15sm)

# Netherlands
load("NLD15sm.RData")
xx <- NLD15sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,NLD15sm)

# Sweden
load("SWE15sm.RData")
xx <- SWE15sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,SWE15sm)

# Portugal
# load("PRT15sm.RData") # NO North Sea TRIPS
# xx <- PRT15sm
# dolphinIndex <-which(xx$sppCode %in% "254960")
# xx[xx$sppCode == "254960",]
# if(length(dolphinIndex)>0)
# {
#   xx <-xx[-dolphinIndex,]
#   cat("Amazon River Dolphin removed from the North Sea \n")
# }
# 
# xx$landCtryFlag <-xx$landCtry
# xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)
# 
# xx$landType <-"own"
# xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
# xx$landTonnage <-xx$landWt/1000
# xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")
# 
# 
# # North Sea data set
# # this comprises all trips that have any landing that are recorded from the wider North Sea areas.  
# 
# NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
# NSareaIndex <-which(xx$area %in% NSarea)
# NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
# NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
# obj <-xx[NStripIndex ,]
# 
# 
# # correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
# #obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
# obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37
# 
# 
# # making a sppType field based on groupings of the ISSCAAP codes
# obj$sppType <-"other"
# 
# obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
# obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
# obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
# obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
# obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
# obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
# obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
# obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
# obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
# obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
# obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"
# 
# 
# 
# # giving each trip a "main" sppType based on the landed weight by sppType code 
# tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
# tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
# tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
# tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
# obj$tripMainSppType <-tripMaxSppTypeChecked
# 
# demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
# demNS <- rbind(demNS, obj[demNorthSeaIndex,])
# rm(xx,obj,PRT15sm)
# 


# UK
load("UK15sm.RData")
xx <- UK15sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS2015 <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,UK15sm)



setwd(path.data)
# save(demNS2015, file = "demNS2015.RData")



#-----------------------------------------
# 2016
#-----------------------------------------
# R is struggling to work with large files, so we save each part as a smaller file and try again
#load("2016_DeriveData.RData")  # has no FRA data

# There is no FRA datain the large derive data set, so it has to be added separately
#FRA_derived_2016 <- readRDS("X:/fishPi2/logbook data/FRA_derived_2016.rds") # get the 2016 FRA data


new.path.data <-"X:/fishPi2/logbook data/JESS indiv ctry working data files/"
setwd(new.path.data)

#FRA16sm <- FRA_derived_2016[,vars]
#save(FRA16sm, file = "FRA16sm.RData")

#BEL16sm <- BEL16[,vars]
#save(BEL16sm, file = "BEL16sm.RData")
#DEU16sm <- DEU16[,vars]
#save(DEU16sm, file = "DEU16sm.RData")
#DNK16sm <- DNK16[,vars]
#save(DNK16sm, file = "DNK16sm.RData")
#ESP16sm <- ESP16[,vars]
#save(ESP16sm, file = "ESP16sm.RData")
#IRL16sm <- IRL16[,vars]
#save(IRL16sm, file = "IRL16sm.RData")
#NLD16sm <- NLD16[,vars]
#save(NLD16sm, file = "NLD16sm.RData")
#SWE16sm <- SWE16[,vars]
#save(SWE16sm, file = "SWE16sm.RData")
#PRT16sm <- PRT16[,vars]
#save(PRT16sm, file = "PRT16sm.RData")
#UK16sm <- UK16[,vars]
#save(UK16sm, file = "UK16sm.RData")

# These files have a reduced number of colmns(17), so that it is easier to work with...

# France
load("FRA16sm.RData")
xx <- FRA16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <-obj[demNorthSeaIndex,]
rm(xx,obj,FRA16sm)

# Belgium
load("BEL16sm.RData")
xx <- BEL16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,BEL16sm)


# Germany
load("DEU16sm.RData")
xx <- DEU16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,DEU16sm)


load("DNK16sm.RData")
xx <- DNK16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,DNK16sm)


# Spain
# load("ESP16sm.RData") # NO fishing trips
# xx <- ESP16sm
# dolphinIndex <-which(xx$sppCode %in% "254960")
# xx[xx$sppCode == "254960",]
# if(length(dolphinIndex)>0)
# {
#   xx <-xx[-dolphinIndex,]
#   cat("Amazon River Dolphin removed from the North Sea \n")
# }
# 
# xx$landCtryFlag <-xx$landCtry
# xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
# xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)
# 
# xx$landType <-"own"
# xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
# xx$landTonnage <-xx$landWt/1000
# xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")
# 
# 
# # North Sea data set
# # this comprises all trips that have any landing that are recorded from the wider North Sea areas.  
# 
# NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
# NSareaIndex <-which(xx$area %in% NSarea)
# NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
# NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
# obj <-xx[NStripIndex ,]
# 
# 
# # correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
# #obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
# obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37
# 
# 
# # making a sppType field based on groupings of the ISSCAAP codes
# obj$sppType <-"other"
# 
# obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
# obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
# obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
# obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
# obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
# obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
# obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
# obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
# obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
# obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
# obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"
# 
# 
# 
# # giving each trip a "main" sppType based on the landed weight by sppType code 
# tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
# tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
# tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
# tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
# obj$tripMainSppType <-tripMaxSppTypeChecked
# 
# demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
# demNS <- rbind(demNS, obj[demNorthSeaIndex,])
# rm(xx,obj,ESP16sm)


# Ireland
load("IRL16sm.RData")
xx <- IRL16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,IRL16sm)

# The Netherlands
load("NLD16sm.RData")
xx <- NLD16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,NLD16sm)

# Sweden
load("SWE16sm.RData")
xx <- SWE16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,SWE16sm)

# Portugal
load("PRT16sm.RData")
xx <- PRT16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,PRT16sm)

# UK
load("UK16sm.RData")
xx <- UK16sm
dolphinIndex <-which(xx$sppCode %in% "254960")
xx[xx$sppCode == "254960",]
if(length(dolphinIndex)>0)
{
  xx <-xx[-dolphinIndex,]
  cat("Amazon River Dolphin removed from the North Sea \n")
}

xx$landCtryFlag <-xx$landCtry
xx$landCtryFlag <-gsub(c("GBE"),c("ENG"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBS"),c("SCT"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBW"),c("WLS"),xx$landCtryFlag)
xx$landCtryFlag <-gsub(c("GBN"),c("NIR"),xx$landCtryFlag)

xx$landType <-"own"
xx$landType[which(xx$landCtryFlag!=xx$vslFlgCtry)] <-"foreign"
xx$landTonnage <-xx$landWt/1000
xx$fleetType <-paste(xx$vslFlgCtry,xx$vslLenCls,sep="_")


# North Sea data set
# this comprises all trips that have any landing that are recorded from the wider North Sea areas.  

NSarea <-c("27.3.a","27.3.a.20","27.3.a.21","27.4.a","27.4.b","27.4.c","27.7.d")
NSareaIndex <-which(xx$area %in% NSarea)
NSareaTrips <-unique(xx$fishTripId[NSareaIndex])
NStripIndex <-which(xx$fishTripId %in% NSareaTrips)
obj <-xx[NStripIndex ,]


# correcting ISSCAAP codes for BoarFish Blue whiting and sandeel to pelagic. 
#obj$tripSppISSCAAP[obj$tripMaxSpp %in% c("BOR","SAN","WHB","NOP")] <-37
obj$ISSCAAP[obj$sppFAO %in% c("BOR","SAN","WHB","NOP")] <-37


# making a sppType field based on groupings of the ISSCAAP codes
obj$sppType <-"other"

obj$sppType[which(obj$ISSCAAP %in% c(31:34))] <-"demersal"
obj$sppType[which(obj$ISSCAAP %in% c(51:56))] <-"mollusca"
obj$sppType[which(obj$ISSCAAP %in% c(57))] <-"cephalopod"
obj$sppType[which(obj$ISSCAAP %in% c(41:47))] <-"crustacea"
obj$sppType[which(obj$ISSCAAP %in% c(35:37))] <-"pelagic"
obj$sppType[which(obj$ISSCAAP %in% c(38))] <-"elasmobranchs"
obj$sppType[which(obj$ISSCAAP %in% c(22:25))] <-"diadromous"
obj$sppType[which(obj$ISSCAAP %in% c(74:83))] <-"benthos"
obj$sppType[which(obj$ISSCAAP %in% c(91:93))] <-"weed"
obj$sppType[which(obj$ISSCAAP %in% c(11:21))] <-"freshwater"
obj$sppType[which(obj$ISSCAAP %in% c(61:64))] <-"mammals"



# giving each trip a "main" sppType based on the landed weight by sppType code 
tripSppTypeWt <-tapply(obj$landWt,list(obj$sppType,obj$fishTripId),sum)
tripMaxSppTypeWt <-apply(tripSppTypeWt,2,max,na.rm=T)
tripMaxSppType <-row.names(tripSppTypeWt)[apply(tripSppTypeWt,2,which.max)]
tripMaxSppTypeChecked <-tripMaxSppType[match(obj$fishTripId,names(tripMaxSppTypeWt))]
obj$tripMainSppType <-tripMaxSppTypeChecked

demNorthSeaIndex <-which(obj$tripMainSppType=="demersal")
demNS2016 <- rbind(demNS, obj[demNorthSeaIndex,])
rm(xx,obj,UK16sm)



setwd(path.data)
save(demNS2016sm, file = "demNS2016.RData")

