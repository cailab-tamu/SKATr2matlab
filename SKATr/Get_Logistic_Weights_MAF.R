Get_Logistic_Weights_MAF <-function(MAF,par1= 0.07, par2=150){

	n<-length(MAF)
	weights<-rep(0,n)
	IDX<-which(MAF > 0)
	if(length(IDX) == 0){
		stop("No polymorphic SNPs")
	} else {

		x1<-(MAF[IDX] - par1) * par2
		weights[IDX]<-exp(-x1)/(1+exp(-x1))
	}

	return(weights)

}
