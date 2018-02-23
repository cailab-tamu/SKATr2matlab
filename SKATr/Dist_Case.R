Dist_Case<-function(eta, Beta0){

	temp<-exp(Beta0 + eta)/(1+exp(Beta0 + eta))
	temp / sum(temp)

}
