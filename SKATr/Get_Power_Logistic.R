Get_Power_Logistic<-function(Z,eta,Beta0,ratio,alpha.ALL,N.Sample.ALL,Weight.Param=c(1,25)){

	Dist<-Dist_Case_Control(eta,Beta0,ratio)
	MAF<-colSums(Z * Dist) /2

	A<-Get_A(Z,eta,Dist,ratio)
	B<-Get_B(Z,eta, Dist,ratio)

	W<-Get_W_New(MAF,Weight.Param)
	A1<-t( t(A) * W)
	B1<-t( t(B) * W)

	c1<-rep(0,4)
	c2<-rep(0,4)
	c_a<-rep(0,4)
	re.all<-list()
	idx<-1

	OUT.Power<-matrix(rep(0,length(alpha.ALL)*length(N.Sample.ALL)),ncol=length(alpha.ALL))
	rownames(OUT.Power)<-N.Sample.ALL
	colnames(OUT.Power)<-alpha.ALL

	for(j in 1:length(N.Sample.ALL)){

		N.Sample<-N.Sample.ALL[j]
		Pi1<-Get_PI_New(MAF,N.Sample)
		W_Pi<-(W*Pi1)

		A2<-t( t(A1 * Pi1) * Pi1)
		diag(A2)<-diag(A2) / Pi1


		K<-t( t(A1) * Pi1)
		K2<-A1 %*% A2

		Mu1<-t( t(B) * W_Pi)
		Mu2<-B1 %*% A2

		c1[1] = trace.SKAT(K) * N.Sample
		c1[2] = trace.SKAT(K2) * N.Sample^2
		c1[3] = sum(K2 * t(K) ) * N.Sample^3
		c1[4] = sum(K2 * t(K2)) * N.Sample^4

		c2[1] = trace.SKAT(  Mu1) * N.Sample^2
		c2[2] = 2 * trace.SKAT( Mu2) * N.Sample^3
		c2[3] = 3 * sum(  Mu2 * t(K)) * N.Sample^4
		c2[4] = 4 * sum(  Mu2 * t(K2)) * N.Sample^5

		#print(c1)
		#print(c2)
		#print(diag(A2))

		for(i in 1:4){
			c_a[i]<-c1[i] + c2[i]
		}
		for(k in 1:length(alpha.ALL)){
			alpha<-alpha.ALL[k]
			out<-Get_Critical_Value(c1, alpha)

			param<-Get_Liu_Params(c_a)
			t = (param$sigmaX/param$sigmaQ)*(out$q.crit-param$muQ) + param$muX
			power<-1-pchisq(t, df = param$l, ncp = param$d)

			#re<-list(N.Sample=N.Sample, alpha=alpha, power=power
			#,q.crit=out$q.crit,out=out,param=param,param.null=out$param.null
			#, c1=c1, c2=c2, c_a = c_a)
			#re.all[[idx]]<-re

			idx= idx + 1
			OUT.Power[j,k]<-power
		}
	}
	#return(re.all)
	return(OUT.Power)
}
