## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----compilnopack,eval=F,include=F---------------------------------------
#  NA

## ----setup---------------------------------------------------------------
library(CLEFRDB)

## ----time1---------------------------------------------------------------
new("Time")

## ----time2---------------------------------------------------------------
print(Timetype)

## ----time3,eval=T--------------------------------------------------------
hauldate<-as.POSIXct(strptime("2011-03-27 01:30:03",
			   "%Y-%m-%d %H:%M:%S"))
new("Time",TimeType="date",TimeDate=hauldate)

## ----time4---------------------------------------------------------------
haultime<-c("2011-03-27 01:30:03",
	    "2011-03-27 12:00:00",
	    "2011-03-15 12:00:00",
	    "2011-02-14 00:00:00")
haultime<-as.POSIXct(strptime(haultime,
			   "%Y-%m-%d %H:%M:%S"))
haultime<-new("Time",TimeType=c("date","day","month","quarter"),
    TimeDate=haultime)
print(haultime)

## ----time5---------------------------------------------------------------
substr(haultime@TimeDate,1,4)

## ----time6,eval=F--------------------------------------------------------
#  new("Time",TimeDate=c(NA),TimeType=c("day"))
#  new("Time",TimeDate=.POSIXct(c(NA,NA)),TimeType=c("day","day"))
#  new("Time",TimeType="date",TimeDate="2011-03-27")
#  hauldate<-as.POSIXct(strptime("2011-03-27 01:30:03",
#  			   "%Y-%m-%d %H:%M:%S"))
#  new("Time",TimeDate=hauldate,TimeType=c("a day"))
#  new("Time",TimeDate=hauldate)

## ----time7,eval=T--------------------------------------------------------
#a Time object related to four haul
        haultime<-c("2011-03-27 01:30:03",
		    "2011-03-27 12:00:00",
		    "2011-03-15 12:00:00",
		    "2011-02-14 00:00:00")
        haultime<-as.POSIXct(strptime(haultime,
				      "%Y-%m-%d %H:%M:%S"))
        haultime<-new("Time",TimeType=c("date","day","month","quarter"),
                          TimeDate=haultime)
# the value of the total landings of the four hauls
w<-c(10000,3000,2000,10)
#Definition of the Landings class
setClass(Class="Landings",
         slots=c("landings"="numeric"),
         contains=c("Time"),
         prototype=prototype(Landings=numeric(),
                             Time=new("Time"))
         )
#a new Landings object
new("Landings")
#a new Landings object initialized with the previous values
initinherit(new("Landings"),landings=w,Time=haultime)

## ----space1--------------------------------------------------------------
new("Space")

## ----space2--------------------------------------------------------------
print(Spacetype)

## ----space3--------------------------------------------------------------
head(defspace)

## ----space4--------------------------------------------------------------
plot(defspace[1:10,"type"],axes=T)

## ----space5,eval=T-------------------------------------------------------
tripgeo<-new("Space",SpacePlace=c("27.7.f","27.7.g","27.7.h",
				  "27E8","27F0","28F0",
				  "GSA07","GSA08",
				  "FRRTB"),
		  SpaceType=c("ICESdiv","ICESdiv","ICESdiv",
			      "ICESrect","ICESrect","ICESrect",
			      "GSA","GSA",
			      "harbour"))
print(tripgeo)

## ----space6,eval=T-------------------------------------------------------
map(tripgeo,axes=T)

## ----space7,eval=F-------------------------------------------------------
#  new("Space",SpacePlace=c("27.7.h","GSA078"),
#      		SpaceType=c("ICEdiv","GSA"))
#  new("Space",SpacePlace=c("27.7.h","GSA07","FRRTB","DEBRB"),
#      		SpaceType=c("ICESdiv","GSA","harbour"))
#  

## ----space8,eval=T-------------------------------------------------------
#a Time object related to four hauls
        haultime<-c("2011-03-27 01:30:03",
		    "2011-03-27 12:00:00",
		    "2011-03-15 12:00:00",
		    "2011-02-14 00:00:00")
        haultime<-as.POSIXct(strptime(haultime,
				      "%Y-%m-%d %H:%M:%S"))
        haultime<-new("Time",TimeType=c("date","day","month","quarter"),
                          TimeDate=haultime)
# the value of the total landings of the four hauls
w<-c(10000,3000,2000,10)
#the area where the hauls were located
haulspace<-new("Space",SpacePlace=c("27.7.f","27.7.g",
				  "27E8","GSA07"),
		  SpaceType=c("ICESdiv","ICESdiv",
			      "ICESrect","GSA"))
#Definition of the Landings class
setClass(Class="Landings",
         slots=c("landings"="numeric"),
         contains=c("Time","Space"),
         prototype=prototype(landings=numeric(),
                             Time=new("Time"),
                             Space=new("Space"))
         )
#a new Landings object
new("Landings")

#a new Landings object initialized with the previous values
haullanding<-initinherit(new("Landings",landings=w),Time=haultime,Space=haulspace)
print(haullanding)
map(haullanding)
#dimclass(haullanding)

## ----space9,eval=T-------------------------------------------------------
#validity of the landings object
validLandings<-function(object){
	if(any(object@landings<0)){print("negative landings !!");FALSE}else{TRUE}
}
#associate the validation function to the landings object
setValidity("Landings",validLandings)
#the original object is still valid
initinherit(new("Landings"),landings=w,Time=haultime,Space=haulspace)
#a negative value in the landings slot will throw an error
## w<-c(10000,3000,-2000,10)
## initinherit(new("Landings",landings=w),Time=haultime,Space=haulspace)

## ----fishframe1----------------------------------------------------------
#Vessel class
setClass(Class="Vessel",
        slots=c(VesselId="character",
                VesselFlag="character",
                VesselType="character",
                VesselLength="integer",
                VesselPower="integer",
                VesselSize="integer"
                ),
        prototype=prototype(VesselId=character(),
                            VesselFlag=character(),
                            VesselType=character(),
                            VesselLength=integer(),
                            VesselPower=integer(),
                            VesselSize=integer()),
        )
#a fictional vessel
myVessel<-new("Vessel", 
	      VesselId="AAAA123",
	      VesselFlag="FRA",
              VesselType=c("Trawler"),
	      VesselLength=as.integer(20),
	      VesselPower=as.integer(5000),
              VesselSize=as.integer(1000))

## ----fishframe2----------------------------------------------------------
#Sampling class"
setClass(Class="Sampling",
	 slots=c(SamplingId="character",
		 SamplingType="character",
		 SamplingMethod="character",
		 SamplingProject="character",
		 SamplingCountry="character"),
	 prototype=prototype(SamplingId=character(),
			     SamplingType=character(),
			     SamplingMethod=character(),
			     SamplingProject=character(),
			     SamplingCountry=character())
	 )

#a fictionnal sample
mySampling<-new("Sampling",
		SamplingId="SAMP2199",
		SamplingType="S",
		SamplingMethod="Observer",
		SamplingProject="SamplingProjectFRA",
		SamplingCountry="FRA")

## ----fishframe3----------------------------------------------------------
mySpaceDep<-new("Space",SpacePlace=c("FRCOC"),SpaceType=c("harbour"))
mySpaceArr<-new("Space",SpacePlace=c("FRCOC"),SpaceType=c("harbour"))
myTimeDep<-new("Time",
	    TimeDate=as.POSIXct(strptime("2011-03-15 12:16:00","%Y-%m-%d %H:%M:%S")),
	    TimeType="date")
myTimeArr<-new("Time",
	    TimeDate=as.POSIXct(strptime("2011-03-19 04:00:32","%Y-%m-%d %H:%M:%S")),
	    TimeType="date")

## ----fishframe4----------------------------------------------------------
# a new Trip class
setClass(Class="Trip",
        slots=c(nbhaul="integer",
                daysatsea="integer",
		TripId="character",
		TimeDep="Time",
		TimeArr="Time",
		SpaceDep="Space",
		SpaceArr="Space"
                ),
        contains=c("Vessel",
                "Sampling"
                ),
         prototype=prototype(TripId=character(),
			     nbhaul=integer(),
                             daysatsea=integer(),
                             Vessel=new("Vessel"),
                             Sampling=new("Sampling"),
                             TimeDep=new("Time"),
                             TimeArr=new("Time"),
                             SpaceDep=new("Space"),
                             SpaceArr=new("Space")
                             )
)

myTrip<-initinherit(new("Trip",
			TripId="FR872",
			nbhaul=as.integer(12),
			daysatsea=as.integer(5)),
		    Sampling=mySampling,
		    Vessel=myVessel,
		    TimeDep=myTimeDep,
		    TimeArr=myTimeArr,
		    SpaceDep=mySpaceDep,
		    SpaceArr=mySpaceArr)


## ----fishframe5----------------------------------------------------------
FishframeTrip<-data.frame(`Record type`="TR",
			  `Sampling type`=myTrip@SamplingType,
			  `Landing country`=myTrip@SamplingCountry,
			  `Vessel flag country`=myTrip@VesselFlag,
			  `Year`=substr(myTrip@TimeArr@TimeDate,1,4),
			  `Project`=myTrip@SamplingProject,
			  `Trip code`=myTrip@TripId,
			  `Vessel length`=myTrip@VesselLength,
			  `Vessel power`=myTrip@VesselPower,
			  `Vessel size`=myTrip@VesselSize,
			  `Vessel type`=myTrip@VesselType,
			  `Harbour`=myTrip@SpaceArr@SpacePlace,
			  `Number of hauls`=myTrip@nbhaul,
			  `Days at sea`=myTrip@daysatsea,
			  `Vessel identifier`=myTrip@VesselId,
			  `Sampling country`=myTrip@SamplingCountry,
			  `Sampling method`=myTrip@SamplingMethod)


## ----rdbes1--------------------------------------------------------------
RDBESTrip<-data.frame(`FishingTripID`=myTrip@SamplingId,
		`OnShoreEventID`="",
		`VesselSelectionID `="",
		`VesselDetailsID `=myTrip@VesselId,
		`SampleDetailsId`=myTrip@SamplingId,
		`Record type`="FT",
		`National Fishing trip Code`=myTrip@TripId,
		`Stratification`="",
		`Trip Stratum`="",
		`FishingTripsClustering`="",
		`FishingTripsClusterName`="",
		`Sampler`="",
		`NumberOfHauls`=myTrip@nbhaul,
		`Departure location`=myTrip@SpaceDep@SpacePlace,
		`Depature date`=substr(myTrip@TimeDep@TimeDate,1,10),
		`Depature time`=substr(myTrip@TimeDep@TimeDate,12,19),
		`Arrival location`=myTrip@SpaceArr@SpacePlace,
		`Arrival date`=substr(myTrip@TimeArr@TimeDate,1,10),
		`Arrival time`=substr(myTrip@TimeArr@TimeDate,12,19),
		`FishingTrips Total`="",
		`FishingTrips Sampled`=1,
		`FishingTripSampelProbability`="",
		`Selection method`="",
		`Selection Method Cluster`="",
		`FishingTripsTotClusters`="",
		`FishingTripsSampClusters`="",
		`FishingTripsClustersProb`="")

