Get_W_New<-function(MAF,Weight.Param){

	W<-Beta_Weights(MAF,Weight.Param)^2
	return(W)

}
