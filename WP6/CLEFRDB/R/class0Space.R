#' Validity space method
#'
#' @param object a space object
#' 
validSpace<-function(object){
	#SpaceType<-NULL
	#utils::data(SpaceType,package="fishpi2qc")
	#print(SpaceType)
		check<-TRUE
		if(length(object@SpacePlace) == length(object@SpaceType)){
			check<-TRUE&check
		}else{
			print(paste0("length(SpacePlace)!=length(SpaceType)"))
			check<-FALSE&check
		}
		if(all(object@SpacePlace%in%CLEFRDB::defspace$id)){
			check<-TRUE&check
		}else{
			id<-which(!object@SpacePlace%in%CLEFRDB::defspace$id)
			print(paste0("wrong Space SpacePlace at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		if(all(object@SpaceType%in%CLEFRDB::Spacetype)){
			check<-TRUE&check
		}else{
			id<-which(!object@SpaceType%in%CLEFRDB::Spacetype)
			print(paste0("wrong Space SpaceType at: ",paste0(id,collapse=",")))
			check<-FALSE&check
		}
		return(check)

}
#' Class Space 
#'
#' @slot SpacePlace 
#' @slot SpaceType 
#'
setClass(Class="Space",
	slots=c(SpaceType="character",SpacePlace="character"),
	prototype=prototype(SpacePlace=character(),SpaceType=character()),
	validity=validSpace
	)



#' map
#' @param x blabla
#' @param ... blabla
#' @export
setGeneric("map", function(x,...) standardGeneric("map"))
#define the methods
#' @export
#' @rdname map
setMethod("map","Space",
	  	  function(x,...){
			  typex<-x@SpaceType
			  placex<-x@SpacePlace
			spacex<-CLEFRDB::defspace[CLEFRDB::defspace$type%in%typex,]
		  #print(spacex)
			spacex<-CLEFRDB::defspace[CLEFRDB::defspace$id%in%placex,]
		  #print(spacex)
		  #print(str(spacex))
		        plot(spacex["label"],...)
			  		  #nomslot<-methods::slotNames(object)
			  		  #plot(x=x@TimeDate,y=rep(0,length(x@TimeDate)),xlab="TimeDate",ylab="",...)#,y=x@TimeType)
		  	  }
		  	  )

#if(F){
#library(CLEFRDB)
#new("Space")
#new("Space",SpacePlace="27.7.h",SpaceType="ICESdiv")
#new("Space",SpacePlace=c("27.7.h","GSA07","FRRTB"),SpaceType=c("ICESdiv","GSA","harbour"))
#new("Space",SpacePlace=c("27.7.h","GSA07"),SpaceType=c("ICESdiv","GSA"))
#new("Space",SpacePlace=c("27.7.h","GSA078"),SpaceType=c("ICESdiv","GSA"))
#new("Space",SpacePlace=c("27.7.h","GSA07","FRRTB","DEBRB"),SpaceType=c("ICESdiv","GSA","harbour"))
#library(CLEFRDB)
#aa<-new("Space",SpacePlace="27.7.h",SpaceType="ICESdiv")
#map(aa)
#plot(aa)

#aa<-new("Space",SpacePlace=c("27.7.h","GSA07","FRRTB"),SpaceType=c("ICESdiv","GSA","harbour"))
#aa<-merge(data.frame(type=aa@SpaceType,id=aa@SpacePlace),defspace)#,by.x="SpaceType",by.y="type")
#pipo<-defspace[defspace$type%in%aa@SpaceType & defspace$id%in%aa@SpacePlace,]
#pipo<-defspace[defspace$id%in%aa@SpacePlace,]
#plot(pipo)


#}
