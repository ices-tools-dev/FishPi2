#' Import slot of an object to another object
#' 
#' @param object1 an object to be transfered to object2
#' @param object2 the object in which object1 will be copied 
#'
#' @return the updated object2
importSlots<-function(object1,object2){
	if(F){
	library(CLEFRDB)
	object1<-methods::new("Time")
	object2<-methods::new("Time")
	importSlots(object1,object2)
	}
	class1<-class(object1)
	if(!inherits(object2,class1)){
		stop("object2 has no inheritance of the object1 class")
	}else{
		slot1<-methods::slotNames(object1)
		slot2<-methods::slotNames(object2)
		for(i in slot1){
			methods::slot(object2,i)<-methods::slot(object1,i)
		}
		return(object2)
	}
}

#' Length of slot
#' 
#' @param object an object
#'
#' @return the length of the slots
lengthSlots<-function(object){
	if(F){
	library(CLEFRDB)
	object<-new("Time")
	lengthSlots(object)
	}

	nomslot<-methods::slotNames(object)
 	lengthall<-c()
	for(i in nomslot){
		len0<-length(methods::slot(object,i))
		lengthall<-c(lengthall,len0)
	}
	return(lengthall)
}



#define the methods
#setMethod("length","Time",
#	  function(object){
#		  #nomslot<-methods::slotNames(object)
#		  length(object@TimeDate)
#	  }
#	  )
#setMethod("show","Time",
#	  function(object){
#		  cat(paste0(length(object)," time objects\n"))
#		  cat(paste0(object@TimeDate," ",object@TimeType,"\n"))
#	  }
#	  )
