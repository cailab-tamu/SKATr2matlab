Get_Power_Logistic_R<-function(Z,eta,Beta0,ratio,alpha.ALL,N.Sample.ALL,Weight.Param=c(1,25),r.corr){

	Dist<-Dist_Case_Control(eta,Beta0,ratio)
	MAF<-colSums(Z * Dist) /2

	A<-Get_A(Z,eta,Dist,ratio)
	B<-Get_B(Z,eta, Dist,ratio)

	W<-Get_W_New(MAF,Weight.Param)

	OUT.Power<-matrix(rep(0,length(alpha.ALL)*length(N.Sample.ALL)),ncol=length(alpha.ALL))
	rownames(OUT.Power)<-N.Sample.ALL
	colnames(OUT.Power)<-alpha.ALL

	for(j in 1:length(N.Sample.ALL)){

		N.Sample<-N.Sample.ALL[j]
		Pi1<-Get_PI_New(MAF,N.Sample)
		W_R<-Get_Power_Corr_WR_rho(W,Pi1,r.corr)

		K<-A %*% W_R
		Mu1<-B %*% W_R

		OUT.Power[j,]<-Get_Power_Corr.GetPower(K,Mu1,alpha.ALL,N.Sample)

	}

	return(OUT.Power)
}
