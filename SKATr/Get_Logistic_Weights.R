Get_Logistic_Weights<-function(Z,par1=0.07, par2=150){

	MAF<-Get_MAF(Z)
	re<-Get_Logistic_Weights_MAF(MAF,par1,par2)
	return(re)
}
