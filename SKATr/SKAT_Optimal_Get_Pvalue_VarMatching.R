SKAT_Optimal_Get_Pvalue_VarMatching<-function(Q.all, Z1, r.all, p_all, Q.sim.all, res.moments, method=NULL){

	n.r<-length(r.all)
	n.q<-dim(Q.all)[1]
	p.m<-dim(Z1)[2]

	lambda.all<-list()
	Z2.all<-list()
	for(i in 1:n.r){
		r.corr<-r.all[i]
		R.M<-diag(rep(1-r.corr,p.m)) + matrix(rep(r.corr,p.m*p.m),ncol=p.m)
		L<-chol(R.M,pivot=TRUE)
		Z2.all[[i]]<- Z1 %*% t(L)
	}

	# Get Mixture param
	param.m<-SKAT_Optimal_Param_VarMatching(Z1,r.all,p_all, res.moments,method)

	Each_Info<-SKAT_Optiaml_Each_Q_VarMatching(param.m, Q.all, r.all, Z2.all,p_all, Q.sim.all,method)
	pmin.q<-Each_Info$pmin.q
	pmin.q.sim<-Each_Info$pmin.q.sim

	pval<-rep(0,n.q)
	pval.sim<-rep(0,n.q)

	muQ 	= param.m$param$muQ
	varQ 	= param.m$param$varQ + param.m$VarRemain
	df 	= param.m$param$df
	tau 	= param.m$tau

	#
	# We only calculate one types of p-values to save computing time
	#
	p.val.each=Each_Info$pval
	for(i in 1:n.q){
		# there was bug in this part, and fixed it
		pval[i]<-SKAT_Optimal_PValue_VarMatching(pmin.q[i,], muQ, varQ, df, tau, r.all)
	}

	return(list(p.value=pval, p.val.each=p.val.each))

}
