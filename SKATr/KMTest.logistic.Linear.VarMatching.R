KMTest.logistic.Linear.VarMatching = function(res, Z, X1, kernel, weights = NULL, pi_1, method, res.out,n.Resampling, r.corr, mu, res.moments = NULL){

	n<-length(pi_1)
  	D  = diag(pi_1)

	# Weighted Linear Kernel
	if (kernel == "linear.weighted") {
	    Z = t(t(Z) * (weights))
	}

	# r.corr
	if(r.corr == 1){
		Z<-cbind(rowSums(Z))
	} else if(r.corr > 0){

		p.m<-dim(Z)[2]
		R.M<-diag(rep(1-r.corr,p.m)) + matrix(rep(r.corr,p.m*p.m),ncol=p.m)
		L<-chol(R.M,pivot=TRUE)
		Z<- Z %*% t(L)
	}

  	Q.Temp = t(res)%*%Z
  	Q = Q.Temp %*% t(Q.Temp)/2

  	Q.res = NULL
  	if(n.Resampling > 0){
  		Q.Temp.res = t(res.out)%*%Z
  		Q.res = rowSums(rbind(Q.Temp.res^2))/2
  	}
  	Z1 = (Z * sqrt(pi_1)) - (X1 * sqrt(pi_1))%*%solve(t(X1)%*%(X1 * pi_1))%*% (t(X1) %*% (Z * pi_1))

	Q.sim = NULL
	if(!is.null(res.moments)){

		Q.Temp.res1 = t(cbind(res.moments))%*%Z
  		Q.sim = rowSums(rbind(Q.Temp.res1^2))/2

	}

	Q.all<-c(Q,Q.res)
	p_all<-mu

	type = "Other"
	if(method =="ECP"){
		type = "OnlySim"
	}
	re<-SKAT_PValue_Logistic_VarMatching(Q.all, Z1 /sqrt(2), p_all, Q.sim, type)


	# re$p.value is p-values of aSKAT

	p.value.resampling = NULL
	p.value.noadj.resampling = NULL


	p.value= re$p.value[1]
	if(length(Q.all) > 1){
		p.value.resampling<-re$p.value[-1]
	}

	p.value.noadj= re$p.value.noadj[1]
	if(length(Q.all) > 1){

		p.value.noadj.resampling<-re$p.value.noadj[-1]
	}

	re<-list(p.value = p.value, p.value.resampling = p.value.resampling
, p.value.noadj = p.value.noadj, p.value.noadj.resampling = p.value.noadj.resampling
, Test.Type = method, Q = Q,  Q.resampling = Q.res, param=NULL)

  	return(re)
}
