SKAT_Optimal_PValue_VarMatching<-function(pmin.q, muQ, varQ, df, tau, r.all){


	re<-integrate(SKAT_Optimal_Integrate_Func_VarMatching, lower=0, upper=30, subdivisions=500, pmin.q=pmin.q, muQ=muQ, varQ=varQ, df=df, tau=tau, r.all=r.all, abs.tol = 10^-15)

	pvalue<-1-re[[1]]
	return(pvalue)

}
