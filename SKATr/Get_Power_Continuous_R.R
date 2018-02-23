Get_Power_Continuous_R<-function(Z,eta,alpha.ALL,N.Sample.ALL,Weight.Param=c(1,25),r.corr){

	A<-Get_A_Q(Z)
	B<-Get_B_Q(Z,eta)

	MAF<-colMeans(Z ) /2
	W<-Get_W_New(MAF,Weight.Param)

	#print(MAF)
	#print(Weight.Param)
	OUT.Power<-matrix(rep(0,length(alpha.ALL)*length(N.Sample.ALL)),ncol=length(alpha.ALL))
	rownames(OUT.Power)<-N.Sample.ALL
	colnames(OUT.Power)<-alpha.ALL

	for(j in 1:length(N.Sample.ALL)){

		N.Sample<-N.Sample.ALL[j]
		Pi1<-Get_PI_New(MAF,N.Sample)
		W_R<-Get_Power_Corr_WR_rho(W,Pi1,r.corr)

		#print(sum(Pi1))
		#print(sum(W_R))

		K<-A %*% W_R
		Mu1<-B %*% W_R

		OUT.Power[j,]<-Get_Power_Corr.GetPower(K,Mu1,alpha.ALL,N.Sample)
	}
	return(OUT.Power)

}
