Resampling_FWER_1<-function(P.value, P.value.Resampling, FWER=0.05){

	if(is.matrix(P.value.Resampling) == FALSE){
		stop("P.value.Resampling should be a matrix")
	}
	p.min<-apply(P.value.Resampling,2,min,na.rm=TRUE)
	P.cut<-quantile(p.min,FWER)
	ID<-which(P.value < P.cut)

	if(length(ID) == 0){
		re<-list(result=NULL, n=length(ID) ,ID=NULL)
	} else {
		re<-list(result=P.value[ID], n=length(ID) ,ID=ID)
	}
	return(re)
}
