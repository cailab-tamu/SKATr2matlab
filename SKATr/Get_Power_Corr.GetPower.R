Get_Power_Corr.GetPower<-function(K,Mu,alpha.ALL,N.Sample){

	n.a<-length(alpha.ALL)
	OUT.Power<-rep(0,n.a)

	c1<-rep(0,4)
	c2<-rep(0,4)
	c_a<-rep(0,4)

	K1<-K
	K2<-K %*% K
	K3<-K2 %*% K

	c1[1] = trace.SKAT(K) * N.Sample
	c1[2] = sum(K *t(K)) * N.Sample^2
	c1[3] = sum(K2 * t(K) ) * N.Sample^3
	c1[4] = sum(K2 * t(K2)) * N.Sample^4

	c2[1] = trace.SKAT(  Mu) * N.Sample^2
	c2[2] = 2 * sum(  K * t(Mu)) * N.Sample^3
	c2[3] = 3 * sum(  K2 * t(Mu)) * N.Sample^4
	c2[4] = 4 * sum(  K3 * t(Mu)) * N.Sample^5

	for(i in 1:4){
		c_a[i]<-c1[i] + c2[i]
	}


	for(k in 1:n.a){
		alpha<-alpha.ALL[k]

		out<-Get_Critical_Value(c1, alpha)
		param<-Get_Liu_Params(c_a)

		t = (param$sigmaX/param$sigmaQ)*(out$q.crit-param$muQ) + param$muX
		power<-1-pchisq(t, df = param$l, ncp = param$d)

		OUT.Power[k]<-power
	}
	return(OUT.Power)

}
