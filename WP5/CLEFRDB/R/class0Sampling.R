#' validity Sampling SamplingMethod
#'
#' @param object a time object
#' 
validSampling<-function(object){
	#Samplingtype<-NULL
	#utils::data(Samplingtype,package="fishpi2qc")
	#print(Samplingtype)
		check<-TRUE
		#data length
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
		#SamplingType
		if(all(object@SamplingType%in%Samplingtype)){
			check<-TRUE&check
		}else{
			id<-which(!object@SamplingType%in%Samplingtype)
			print(paste0("wrong SamplingType at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		#SamplingMethod
		if(all(object@SamplingMethod%in%Samplingmethodtype)){
			check<-TRUE&check
		}else{
			id<-which(!object@SamplingMethod%in%Samplingmethodtype)
			print(paste0("wrong SamplingMethod at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		#SamplingProject:not needed
		#SamplingCountry
		if(all(object@SamplingCountry%in%defcountry$id)){
			check<-TRUE&check
		}else{
			id<-which(!object@SamplingCountry%in%defcountry$id)
			print(paste0("wrong SamplingCountry at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		return(check)

}
#' Class Sampling
#'
#' @slot type 
#' @slot SamplingMethod 
#' @slot SamplingProject 
#' @slot SamplingCountry 
#'
setClass(Class="Sampling",
	slots=c(SamplingType="character",SamplingMethod="character",SamplingProject="character",SamplingCountry="character"),
	prototype=prototype(SamplingType=character(),SamplingMethod=character(),
			    SamplingProject=character(),SamplingCountry=character()),
	validity=validSampling
	)
if(F){
	library()
load("../data/Samplingtype.rda")
load("../data/Samplingmethodtype.rda")
load("../data/defcountry.rda")
new("Sampling")
new("Sampling",SamplingCountry="FRA",SamplingType="M")
new("Sampling",SamplingCountry="FRA",SamplingType="U")
}
