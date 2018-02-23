Resampling_FWER<-function(obj,FWER=0.05){

	if(class(obj) != "SKAT_SSD_ALL"){
		stop("obj is not a SKAT.SSD.All output object")
	}
	p.min<-apply(obj$P.value.Resampling,2,min,na.rm=TRUE)
	P.cut<-quantile(p.min,FWER)
	ID<-which(obj$results$P.value < P.cut)

	if(length(ID) == 0){
		re<-list(result=NULL, n=length(ID) ,ID=NULL)
	} else {
		re<-list(result=obj$results[ID,], n=length(ID) ,ID=ID)
	}
	return(re)

}
