Get_Resampling_Pvalue_1<-function(p.value,p.value.resampling){

	if(is.null(p.value.resampling)){
		stop("No resampling was applied!")
	}

	n<-length(p.value.resampling)
	n1<-length(which(p.value >= p.value.resampling))
	pval1<-(n1+1)/(n+1)

	re<-list(p.value=pval1, is_smaller=FALSE)
	if(n1==0){
		re$is_smaller=TRUE
	}

	return(re)
}
