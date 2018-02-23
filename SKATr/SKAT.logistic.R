SKAT.logistic = function(Z,y,X1, kernel = "linear", weights = NULL, method="liu"
, res.out=NULL, n.Resampling = 0, r.corr=r.corr){


	n = length(y)
	m = ncol(Z)

	glmfit= glm(y~X1 -1, family = "binomial")
 	betas = glmfit$coef
  	mu    = glmfit$fitted.values
  	eta   = glmfit$linear.predictors

	mu    = glmfit$fitted.values
	pi_1 = mu*(1-mu)
  	res = y- exp(eta)/(1+exp(eta))

	if(method=="var.match"){
		re = KMTest.logistic.Linear.VarMatching(res, Z, X1, kernel, weights, pi_1, method,res.out,n.Resampling,r.corr, mu)
		return(re)
	}

	# If m >> p and ( linear or linear.weight) kernel than call
	# Linear function
	if( (kernel =="linear" || kernel == "linear.weighted") && n > m){
		re = SKAT.logistic.Linear(res,Z,X1, kernel, weights , pi_1,method,res.out,n.Resampling,r.corr=r.corr)
	} else {
		re = SKAT.logistic.Other(res,Z,X1, kernel, weights, pi_1, method,res.out,n.Resampling)
	}

	return(re)
}
