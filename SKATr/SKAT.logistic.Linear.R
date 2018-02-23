SKAT.logistic.Linear = function(res,Z,X1, kernel, weights = NULL, pi_1, method,res.out,n.Resampling,r.corr){

	if(length(r.corr) > 1 && dim(Z)[2] == 1){
		r.corr=0
	}

	if(length(r.corr) == 1){

		re = KMTest.logistic.Linear(res,Z,X1, kernel, weights, pi_1, method
		, res.out, n.Resampling, r.corr)

	} else {

		re =SKAT_Optimal_Logistic(res, Z, X1, kernel, weights, pi_1, method
		, res.out, n.Resampling, r.corr)

	}

	return(re)
}
