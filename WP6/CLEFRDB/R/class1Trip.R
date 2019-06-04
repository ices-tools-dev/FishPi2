#' validity Trip method
#'
#' @param object a time object
#' 
validTrip<-function(object){
	#Triptype<-NULL
	#utils::data(Triptype,package="fishpi2qc")
	#print(Triptype)
		check<-TRUE
		#data length
#if(F){
#object<-new("Trip")
		nomslot<-methods::slotNames(object)
		lengthall<-c()
		for(i in nomslot){
			len0<-length(methods::slot(object,i))
			lengthall<-c(lengthall,len0)
		}
		if(all(lengthall[1]==lengthall)){
			check<-TRUE&check
		}else{
			print(paste0("lengths of parameters are not equal"))
			check<-FALSE&check
		}
		#type
		if(all(object@type%in%CLEFRDB::Triptype)){
			check<-TRUE&check
		}else{
			id<-which(!object@type%in%CLEFRDB::Triptype)
			print(paste0("wrong type at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		#method
		if(all(object@method%in%CLEFRDB::Tripmethodtype)){
			check<-TRUE&check
		}else{
			id<-which(!object@method%in%CLEFRDB::Tripmethodtype)
			print(paste0("wrong method at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		#project:not needed
		#country
		if(all(object@country%in%CLEFRDB::defcountry$id)){
			check<-TRUE&check
		}else{
			id<-which(!object@country%in%CLEFRDB::defcountry$id)
			print(paste0("wrong country at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
#}
		return(check)

}
#' Class Trip
#'
#' @slot vessel 
#' @slot sampling 
#' @slot time 
#' @slot space 
#' @slot nbhaul 
#' @slot daysatsea 
#'
setClass(Class="Trip",
	slots=c(nbhaul="integer",
		daysatsea="integer"
		),
	contains=c("Vessel",
		"Sampling",
		"Time",
		"Space"
		),
	 prototype=prototype(nbhaul=integer(),
			     daysatsea=integer(),
			     Vessel=methods::new("Vessel"),
			     Sampling=methods::new("Sampling"),
			     Time=methods::new("Time"),
			     Space=methods::new("Space")
			     ),
	validity=validTrip
)

setMethod("initialize","Trip",function(.Object,...){
if(F){
#	dots<-list(Space=new("Space",SpaceType="ICESdiv",SpacePlace="27.7.g"),Time=new("Time",TimeDate=Sys.time(),TimeType="date"))
# 	.Object<-methods::new("Trip")
}
  dots<-list(...)
  if(length(dots)>0){
  	testusedots<-lapply(dots,function(a){a<-FALSE})
	#class inheritance in value
	for(namedots in names(dots)){
		#class to class
		#print(namedots)
		if(inherits(.Object,namedots)){
			#print("class")
 			testusedots[[namedots]]<-TRUE
  			slotobj<-methods::slotNames(.Object)
			slotdots<-methods::slotNames(dots[[namedots]])
			for(idslot in slotdots){
				print(idslot)
				methods::slot(.Object,idslot)<-methods::slot(dots[[namedots]],idslot)
			}
		}
		#slot to slot
		if(any(methods::slotNames(.Object)%in%namedots)){
			#print("slot")
 			testusedots[[namedots]]<-TRUE
			methods::slot(.Object,namedots)<-dots[[namedots]]
		}
	}
	wrongdots<-unlist(testusedots)
	wrongdots<-names(wrongdots)[!wrongdots]
#print(wrongdots)
	#print(unlist(testusedots))
	#if(any(unlist(testusedots))){
	if(length(wrongdots)>0){
		wrongdots<-unlist(testusedots)
		wrongdots<-names(wrongdots)[!wrongdots]
		warning(paste0("parameters ",paste0(wrongdots,collapse=",")," unknown not used"))
	}
  }
  return(.Object)
})


#if(F){
#
#	library(CLEFRDB)
#	source("00function.R")
#	aa<-new("Trip")
#	aa<-new("Trip",VesselId="date")
#	aa<-new("Trip",VesselId="geooorges",robert="jjj")
#	aa<-new("Trip",robert="jjj")
#	pipo<-new("Trip")
#	tt<-new("Vessel",id=10)
#	new("Trip",vessel=tt)
#	pipo
#
#	load("../data/Triptype.rda")
#	load("../data/Tripmethodtype.rda")
#	load("../data/defcountry.rda")
#	pipo<-new("Time",TimeType="youlgi")
#	pipo@TimeType<-"oupu"
#	new("Trip",Time=pipo)
#	new("Trip",country="FRA",type="M")
#	new("Trip",country="FRA",type="U")
#setClass(Class="pipo",
#	slots=c(nbhaul="integer",
#		daysatsea="integer"
#		),
#	contains=c("Vessel",
#		"Space","Time"
#		),
#	#validity=validTrip
#	)
#
#setMethod("initialize", "pipo",
#    function(.Object,
#	     vessel=new("Vessel"),
#	     space=new("Space"),
#	     time=new("Time"),
#	     nbhaul=integer(),
#	     daysatsea=integer(),
#	     ...){
#	    .Object <- methods::callNextMethod()
#	    .Object<-importSlots(vessel,.Object)
#	    .Object<-importSlots(space,.Object)
#	    .Object<-importSlots(time,.Object)
#	    .Object@nbhaul<-nbhaul
#	    .Object@daysatsea<-daysatsea
#	    #methods::validObject(.Object)
#	    return(.Object)
#    }
#   ) 
#
#load("../data/Timetype.rda")
#source("00function.R")
#source("class0Time.R")
#source("class0Vessel.R")
#source("class0Space.R")
#new("pipo")
#
#}
