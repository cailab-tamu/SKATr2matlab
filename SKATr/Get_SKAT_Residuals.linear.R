Get_SKAT_Residuals.linear = function(formula, data, n.Resampling, type.Resampling, id_include ){


 	mod = lm(y.c ~ X)
	X1<-model.matrix(lm(y.c ~ X))

  	s2 = summary(mod)$sigma**2
  	res = mod$resid
	n1<-length(res)
	res.out<-NULL

	if(n.Resampling > 0){

		if(type.Resampling=="permutation"){
			res.out<-res %x% t(rep(1,n.Resampling))
			res.out<-apply(res.out,2,sample)
		} else if(type.Resampling=="bootstrap"){
			res.out<-matrix(rnorm(n1*n.Resampling,mean=0,sd=sqrt(s2)),ncol=n.Resampling)
			X1_inv<-solve(t(X1) %*% X1)
			res.out<- res.out - (X1 %*% X1_inv) %*% (t(X1) %*% res.out)
		} else if(type.Resampling=="perturbation"){
			res.out<-matrix(rnorm(n1*n.Resampling,mean=0,sd=1),ncol=n.Resampling)
			res.out<-res.out * res
			stop("Error: Perturbation is no more provided!")
		} else {
			stop("Error: Wrong resampling method!")
		}
	}

  	return(list(res=res, X1=X1,res.out=res.out,out_type="C",
	n.Resampling=n.Resampling, type.Resampling=type.Resampling,
	id_include=id_include, s2=s2))
}
