Get_Resampling_Pvalue<-function(obj){

	if(class(obj) != "SKAT_OUT"){
		stop("obj is not a SKAT output object")
	}

	if(is.null(obj$p.value.resampling)){
		stop("No resampling was applied!")
	}

	n<-length(obj$p.value.resampling)
	n1<-length(which(obj$p.value >= obj$p.value.resampling))
	pval1<-(n1+1)/(n+1)

	re<-list(p.value=pval1, is_smaller=FALSE)
	if(n1==0){
		re$is_smaller=TRUE
	}

	return(re)
}
