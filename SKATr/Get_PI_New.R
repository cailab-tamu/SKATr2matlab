Get_PI_New<-function(MAF,N.Sample){

	Pi_1<- 1 - (1 - MAF)^(2 * N.Sample)
	return(Pi_1)

}
