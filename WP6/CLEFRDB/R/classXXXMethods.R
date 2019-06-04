#' initinherit
#'
#' a generic function to initialize object
#' @param Object an object
#' @param ... the object or parameters from which inheritance is transmitted
#' @return the update object
#'
#' @export
initinherit<-function(Object,...){
  #Object <- callNextMethod()
  dots<-list(...)
  if(length(dots)>0){
  	testusedots<-lapply(dots,function(a){a<-FALSE})
	#class inheritance in value
	for(namedots in names(dots)){
		#class to class
		#print(namedots)
		if(inherits(Object,namedots)){
			#print("class")
 			testusedots[[namedots]]<-TRUE
  			slotobj<-methods::slotNames(Object)
			slotdots<-methods::slotNames(dots[[namedots]])
			for(idslot in slotdots){
				#print(idslot)
				methods::slot(Object,idslot)<-methods::slot(dots[[namedots]],idslot)
			}
		}
		#slot to slot
		if(any(methods::slotNames(Object)%in%namedots)){
			#print("slot")
 			testusedots[[namedots]]<-TRUE
			methods::slot(Object,namedots)<-dots[[namedots]]
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
	#test slot size
	newslots<-methods::slotNames(Object)
	lenslots<-c()
	for(i in newslots){
		lenslots<-c(lenslots,length(methods::slot(Object,i)))
  	}
	if(length(unique(lenslots))!=1){
		print("slots have differents length:")
		rez<-data.frame(name=newslots,length=lenslots)
		print(rez)
		stop("!!!!")
	}
  }
  return(Object)
}


#define the methods
setMethod("plot","Time",
	  function(x,...){
		  #nomslot<-methods::slotNames(object)
		  plot(x=x@TimeDate,y=rep(0,length(x@TimeDate)),xlab="TimeDate",ylab="",...)#,y=x@TimeType)
	  }
	  )
#setMethod("show","Time",
#	  function(object){
#		  cat(paste0(length(object)," time objects\n"))
#		  cat(paste0(object@TimeDate," ",object@TimeType,"\n"))
#	  }
#	  )

#' dimension 
#'
#' a generic function to have the dimensiob object
#' @param Object an object
#' @param ... the object or parameters from which inheritance is transmitted
#' @return the update object
#'
#' @export
#' @rdname dimension
dimclass<-function(Object){
	if(F){
#		haultime->Object
	}
  #Object <- callNextMethod()
	nomslots<-methods::slotNames(Object)
	#slot(Object,"TimeType")#nomslots[1])
	nbcol<-length(nomslots)
	nbrow<-length(methods::slot(Object,nomslots[1]))
	c(nbrow,nbcol)
	#setMethod("dim","Time",function(x,...)
}
#' generic dim by class
#' @param x an Object
#' @export
setMethod("dim","Time", function(x) dimclass(x))
#' generic dim by class
#' @param x an Object
#' @export
setMethod("dim","Space", function(x) dimclass(x))
#' generic dim by class
#' @param x an Object
#' @export
setMethod("dim","Vessel", function(x) dimclass(x))

#' dimension of slots
#'
#' a generic function to have the length of each slot in an object
#' @param Object an object
#' @param ... the object or parameters from which inheritance is transmitted
#' @return the update object
#'
#' @export
dimslots<-function(Object){
	if(F){
#		haultime->Object
	}
  #Object <- callNextMethod()
	nomslots<-methods::slotNames(Object)
	lengthslots<-rep(NA,length(nomslots))
	#slot(Object,"TimeType")#nomslots[1])
	for(i in seq_along(nomslots)){
		    lengthslots[i]<-length(methods::slot(Object,nomslots[i]))
	}
	return(lengthslots)
	nbcol<-length(nomslots)
	nbrow<-length(methods::slot(Object,nomslots[1]))
	c(nbrow,nbcol)
	#setMethod("dim","Time",function(x,...)
}
#' generic dim by class
setMethod("dim","Time", function(x) dimclass(x))
#' generic dim by class
setMethod("dim","Space", function(x) dimclass(x))
setMethod("dim","Vessel", function(x) dimclass(x))

#' coerce to data frame 
#'
#' a generic function to have the dimensiob object
#' @param Object an object
#' @param ... the object or parameters from which inheritance is transmitted
#' @return the update object
#'
#' @export
as.df<-function(Object){
	if(F){
#		haultime->Object
#		lapply(Object,names)
#	lapply(Object,slot)#,nomslots)
#	nomslots<-slotNames(Object)
#	lapply(Object,dim)
#	lapply(Object, FUN=slot(Object,name=nomslots))
#	slot(Object,nomslots[1])
#	fct1<-function(a,b){slot(a,b)}
#	slot(haultime,"TimeType")
#	fct1(haultime,"TimeType")
#	sapply(haultime,fct1,b="TimeType")
#
	}
  #Object <- callNextMethod()
	dimobj<-dim(Object)
	nomslots<-methods::slotNames(Object)
	df<-data.frame(matrix(nrow=dimobj[1],ncol=dimobj[2]))
	names(df)<-nomslots
	#slot(Object,"TimeType")#nomslots[1])
	for(i in seq_along(nomslots)){
		df[,i]<-methods::slot(Object,nomslots[i])
		#df[i,]
		#names(dftmp)<-nomslots[i]
		#df<-cbind(df,dftmp)
	}
	return(df)
}



#if(F){
#new("Vessel")%>%dim
#new("Space")%>%as.df
#		  #nomslot<-methods::slotNames(object)
#		  #plot(x=x@TimeDate,y=rep(0,length(x@TimeDate)),xlab="TimeDate",ylab="",...)#,y=x@TimeType)
#
#
#
#	library("CLEFRDB")
#	library(dplyr)
#	new("Time")
#	new("Time")%>%dim
#	new("Space")%>%dimclass
#	new("Space")%>%dim
#
#	library("CLEFRDB")
#	haultime<-c("2011-03-27 01:30:03",
#		                "2011-03-27 12:00:00",
#				            "2011-03-15 12:00:00",
#				            "2011-02-14 00:00:00")
#	haultime<-as.POSIXct(strptime(haultime,
#				                                 "%Y-%m-%d %H:%M:%S"))
#	haultime<-new("Time",TimeType=c("date","day","month","quarter"),
#		          TimeDate=haultime)
#	print(haultime)
#	dimclass(haultime)
#	dim(haultime)
#	as.df(haultime)
#
#setClass(Class="Landings",
#	 slots=c("w"="numeric"),
#	 contains=c("Time"),
#	 prototype=prototype(w=numeric(),
#			     Time=new("Time"))
#	 )
#
#new("Landings")
#new("Landings",w=rnorm(4),TimeDate=haultime@TimeDate,TimeType=haultime@TimeType)
#
#setMethod("initialize","Landings",function(.Object){.Object<-initinherit(.Object);return(.Object)})
#new("Landings")
#new("Landings",w=rnorm(4),Time=haultime)
#
#initinherit(new(""),w=1:4,Time=haultime,pipo=10)
#aa<-initinherit(new("Landings"),w=c(10,20,50,60,0),Time=haultime)
#new("Landings",Landings=rnorm(4))#,Time=haultime)
#new("Trip",Time=haultime)
#
#
#}
