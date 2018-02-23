Beta_Weights<-function(MAF,Weight.Param){


	p<-length(MAF)
	w<-rep(0,p)
	IDX<-which(MAF > 0)

	w[IDX]<-dbeta(MAF[IDX],Weight.Param[1],Weight.Param[2])
	return(w)
}
