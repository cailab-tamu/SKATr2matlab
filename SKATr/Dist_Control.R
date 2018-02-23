Dist_Control<-function(eta,Beta0){

	temp<-1/(1+exp(Beta0 + eta))
	temp / sum(temp)

}
