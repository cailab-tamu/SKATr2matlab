SKAT_Optimal_PValue_Davies<-function(pmin.q,param.m,r.all){

	re<-try(integrate(SKAT_Optimal_Integrate_Func_Davies, lower=0, upper=30, subdivisions=500,pmin.q=pmin.q,param.m=param.m,r.all=r.all,abs.tol = 10^-15), silent = TRUE)
	if(class(re) == "try-error"){
		re<-SKAT_Optimal_PValue_Liu(pmin.q,param.m,r.all)
		return(re)
	}

	pvalue<-1-re[[1]]
	if(pvalue < 0){
		pvalue=0
	}
	return(pvalue)

}
