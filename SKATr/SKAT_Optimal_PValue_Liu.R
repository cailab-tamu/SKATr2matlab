SKAT_Optimal_PValue_Liu<-function(pmin.q,param.m,r.all){

	 re<-integrate(SKAT_Optimal_Integrate_Func_Liu, lower=0, upper=30, subdivisions=500
	,pmin.q=pmin.q,param.m=param.m,r.all=r.all,abs.tol = 10^-15)

	pvalue<-1-re[[1]]
	return(pvalue)

}
