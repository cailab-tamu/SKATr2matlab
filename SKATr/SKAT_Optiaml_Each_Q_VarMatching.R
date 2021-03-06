SKAT_Optiaml_Each_Q_VarMatching<-function(param.m, Q.all, r.all, Z2.all, p_all, Q.sim.all, method="Other"){

	type = "Other"
	if(method == "ECP"){
		type = "OnlySim"
	}

	Is.SIM<-!is.null(Q.sim.all)
	n.r<-length(r.all)
	c1<-rep(0,4)
	n.q<-dim(Q.all)[1]

	pval<-matrix(rep(0,n.r*n.q),ncol=n.r)
	pval.sim<-matrix(rep(0,n.r*n.q),ncol=n.r)

	pmin.q<-matrix(rep(0,n.r*n.q),ncol=n.r)
	pmin.q.sim<-matrix(rep(0,n.r*n.q),ncol=n.r)

	re.param<-list()
	for(i in 1:n.r){
		Q<-Q.all[,i]
		r.corr<-r.all[i]

		out<-SKAT_PValue_Logistic_VarMatching(Q, Z2.all[[i]], p_all, Q.sim.all[,i],type)

		re.param[[i]]<-out$param
		pval[,i]<- out$p.value

	}

	pmin<-apply(pval,1,min)

	# re-adjust the kurtosis of Q using the estimated kurtosis
	for(i in 1:n.r){
		r.corr<-r.all[i]
		muQ<-re.param[[i]]$muQ
		varQ1<-re.param[[i]]$varQ
		varQ<-(1-r.corr)^2*(param.m$param$varQ + param.m$VarRemain) + param.m$tau[i]^2*2

		#print(c(varQ, varQ1, param.m$tau[i]^2*2))

		df<-re.param[[i]]$df

		vq1<-param.m$param$varQ + param.m$VarRemain
		ker<-SKAT_Optimal_Get_Kertosis_Mixture(param.m$param$df, 1, vq1 , (1-r.corr), param.m$tau[i])


		df<-12/ker

		q.org<-qchisq(1-pmin,df=df)
		q.q<-(q.org - df)/sqrt(2*df) *sqrt(varQ) + muQ
		pmin.q[,i]<-q.q

	}
	out<-list(pmin=pmin, pval=pval, pmin.q=pmin.q)
	return(out)

}
