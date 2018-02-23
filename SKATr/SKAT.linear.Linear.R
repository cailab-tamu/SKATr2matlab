SKAT.linear.Linear = function(res,Z,X1, kernel, weights = NULL, s2, method,res.out,n.Resampling,r.corr){

	if(length(r.corr) > 1 && dim(Z)[2] == 1){
		r.corr=0
	}

	if(length(r.corr) == 1){

		re = KMTest.linear.Linear(res,Z,X1, kernel, weights, s2, method
		, res.out, n.Resampling, r.corr)

	} else {

		re =SKAT_Optimal_Linear(res, Z, X1, kernel, weights, s2, method
		, res.out, n.Resampling, r.corr)
	}
	return(re)
}
