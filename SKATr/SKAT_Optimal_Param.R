SKAT_Optimal_Param<-function(Z1,r.all){


	n<-dim(Z1)[1]
	p.m<-dim(Z1)[2]
	r.n<-length(r.all)

	z_mean<-rowMeans(Z1)
	Z_mean<-matrix(rep(z_mean,p.m),ncol=p.m,byrow=FALSE)
	cof1<-(t(z_mean) %*% Z1)[1,] / sum(z_mean^2)

	Z.item1<-Z_mean %*% diag(cof1)
	Z.item2<-Z1 - Z.item1

	# W3.2 Term : mixture chisq
	W3.2.t<-t(Z.item2) %*% Z.item2
	lambda<-Get_Lambda(W3.2.t)

	# W3.3 Term : variance of remaining ...
	W3.3.item<-sum((t(Z.item1) %*% Z.item1) * (t(Z.item2) %*% Z.item2)) * 4

	# Mixture Parameters
	MuQ<-sum(lambda)
	VarQ<-sum(lambda^2) *2 + W3.3.item
	KerQ<-sum(lambda^4)/(sum(lambda^2))^2 * 12
	Df<-12/KerQ

	# W3.1 Term : tau1 * chisq_1
	tau<-rep(0,r.n)
	for(i in 1:r.n){
		r.corr<-r.all[i]
		#term1<-p.m*r.corr + cof1^2 * (1-r.corr)
		term1<-p.m^2*r.corr + sum(cof1^2) * (1-r.corr)
		tau[i]<-sum(term1) *  sum(z_mean^2)
	}

	out<-list(MuQ=MuQ,VarQ=VarQ,KerQ=KerQ,lambda=lambda,VarRemain=W3.3.item,Df=Df,tau=tau)
	return(out)
}
