#' validity time method
#'
#' @param object a time object
#' 
validTime<-function(object){
	#TimeTimeType<-NULL
	#utils::data(TimeTimeType,package="fishpi2qc")
	#print(TimeTimeType)
		check<-TRUE
		if(length(object@TimeDate) == length(object@TimeType)){
			check<-TRUE&check
		}else{
			print(paste0("length(TimeDate)!=length(TimeType)"))
			check<-FALSE&check
		}
		if(!anyNA(object@TimeDate)){
			check<-TRUE&check
		}else{
			id<-which(is.na(object@TimeDate))
			print(paste0("NA TimeDate at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		if(all(object@TimeType%in%Timetype)){
			check<-TRUE&check
		}else{
			id<-which(!object@TimeType%in%Timetype)
			print(paste0("wrong Time TimeType at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		return(check)

}
#' Class Time
#'
#' @slot TimeType 
#' @slot TimeDate 
#'
setClass(Class="Time",
	slots=c(TimeType="character",TimeDate="POSIXct"),
	prototype=prototype(TimeType=character(),TimeDate=.POSIXct(numeric())),
	validity=validTime
)
if(F){
	library(CLEFRDB)
aday<-as.POSIXct(strptime("2011-03-27 01:30:03", "%Y-%m-%d %H:%M:%S"))
new("Time")
str(new("Time"))
new("Time",TimeDate=aday,TimeType="day")
new("Time",TimeDate=aday,TimeType="day")
str(new("Time",TimeDate=c(aday,aday),TimeType=c("year","day")))
new("Time",TimeDate=c(aday,NA),TimeType=c("day","day"))
new("Time",TimeDate=.POSIXct(c(NA,NA)),TimeType=c("day","day"))
new("Time",TimeDate=c(aday,aday),TimeType=c("day"))
new("Time",TimeDate=c(aday,NA,NA),TimeType=c("pipi","day","day"))
new("Time",TimeDate=c(aday,aday,NA,NA),TimeType=c("day","year","semaine"))

#example with methods ?


}
