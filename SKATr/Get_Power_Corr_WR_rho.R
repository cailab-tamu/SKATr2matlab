Get_Power_Corr_WR_rho<-function(W,Pi,r.corr){

	n<-length(W)
	R<-matrix(rep(1,n*n),ncol=n) * r.corr
	diag(R)<-rep(1,n)

	temp<-sqrt(W) * Pi

	W_rho<- t(t(R * temp) * temp)
	diag(W_rho)<-W * Pi

	return(W_rho)

}
