Get_PI_W_ALL<-function(MAF,N.Sample,Weight.Param){

	W<-Beta_Weights(MAF,Weight.Param)^2
	Pi<- 1 - (1 - MAF)^(2 * N.Sample)
	W_Pi<-W * Pi

	return(W_Pi)

}
