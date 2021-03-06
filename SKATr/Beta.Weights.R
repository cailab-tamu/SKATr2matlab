Beta.Weights<-function(MAF,weights.beta){

	n<-length(MAF)
	weights<-rep(0,n)
	IDX_0<-which(MAF == 0)
	if(length(IDX_0) == n){
		stop("No polymorphic SNPs")
	} else if( length(IDX_0) == 0){
		weights<-dbeta(MAF,weights.beta[1],weights.beta[2])
	} else {
		weights[-IDX_0]<-dbeta(MAF[-IDX_0],weights.beta[1],weights.beta[2])
	}

	#print(length(IDX_0))
	#print(weights[-IDX_0])
	return(weights)

}
