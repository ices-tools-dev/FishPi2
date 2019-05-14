#' ValVesselIdity Vessel method
#'
#' @param object a vessel object
#' 
validVessel<-function(object){
	#VesselVesselType<-NULL
	#utils::data(VesselVesselType,package="fishpi2qc")
	#print(VesselVesselType)
		check<-TRUE
		#data VesselLength
		nomslot<-methods::slotNames(object)
		VesselLengthall<-c()
		for(i in nomslot){
			len0<-length(methods::slot(object,i))
			VesselLengthall<-c(VesselLengthall,len0)
		}
		if(all(VesselLengthall[1]==VesselLengthall)){
			check<-TRUE&check
		}else{
			print(paste0("VesselLengths of vessel parameters are not equal"))
			check<-FALSE&check
		}
		#VesselId: not needed
		#VesselFlag
		if(all(object@VesselFlag%in%defcountry$id)){
			check<-TRUE&check
		}else{
			VesselId<-which(!object@VesselFlag%in%defcountry$id)
			print(paste0("wrong country at: ",paste0(VesselId,collapse=",")))
			check<-FALSE&check
		}
		#VesselLength
		if(all(object@VesselLength>0)){
			check<-TRUE&check
		}else{
			VesselId<-which(object@VesselLength<=0)
			print(paste0("wrong VesselLength: ",paste0(VesselId,collapse=",")))
			check<-FALSE&check
		}
		#VesselPower
		if(all(object@VesselPower>0)){
			check<-TRUE&check
		}else{
			VesselId<-which(object@VesselPower<=0)
			print(paste0("wrong VesselPower: ",paste0(VesselId,collapse=",")))
			check<-FALSE&check
		}
		#VesselSize
		if(all(object@VesselSize>0)){
			check<-TRUE&check
		}else{
			VesselId<-which(object@VesselSize<=0)
			print(paste0("wrong VesselSize: ",paste0(VesselId,collapse=",")))
			check<-FALSE&check
		}
		return(check)

}
#' Class Vessel 
#'
#' @slot VesselId 
#' @slot VesselFlag 
#' @slot VesselType 
#' @slot VesselLength 
#' @slot VesselPower 
#' @slot VesselSize 
#'
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
	validity=validVessel
	)
if(F){
library(CLEFRDB)
new("Vessel")
new("Vessel",VesselId="pipo",VesselFlag="FRA",VesselLength=as.integer(10),VesselPower=as.integer(10))
}
