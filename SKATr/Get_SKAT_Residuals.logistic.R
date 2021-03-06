Get_SKAT_Residuals.logistic = function(formula, data, n.Resampling, type.Resampling,id_include){


 	mod = lm(formula, data)
	X1<-model.matrix(formula,data=data)

	glmfit= glm(formula, data=data, family = "binomial")
 	betas = glmfit$coef
  	mu    = glmfit$fitted.values
  	eta   = glmfit$linear.predictors
	n.case = sum(glmfit$y)

	pi_1 = mu*(1-mu)
  	res = glmfit$y- mu
	n1<-length(res)
	res.out<-NULL

	if(n.Resampling > 0){
		if(type.Resampling=="permutation"){
			res.out1<-res %x% t(rep(1,n.Resampling))
			res.out<-apply(res.out1,2,sample)
		} else if(type.Resampling=="bootstrap"){
			mu1<-mu/sum(mu)	# all prob
			res.out<-matrix(rep(0,n.Resampling*n1),ncol=n.Resampling)
			for(i in 1:n.Resampling){
				#id_case<-sample(1:n1,n.case,prob=mu1)
				#res.out[id_case,i]<-1
				#res.out[,i]<-rbinom(n1,1,mu)

				res.out1<-rbinom(n1,1,mu)
				res.out2<-rbinom(n1,1,mu)

				id_case1<-which(res.out1 ==1)
				id_case2<-which(res.out2 ==1)

				id_c1<-intersect(id_case1,id_case2)
				id_c2<-union(setdiff(id_case1,id_case2),setdiff(id_case2,id_case1))
				if(n.case <= length(id_c1)){
					id_case<-sample(id_c1,n.case)
				}else if (n.case > length(id_c1) && n.case <= length(id_c1)+length(id_c2)){
					id_c3<-sample(id_c2,n.case - length(id_c1))
					id_case<-c(id_c1,id_c3)
				}else {
					id_case3<-union(id_c1,id_c2)
					id_c4<-setdiff(1:n1,id_case3)
					n.needed<-n.case - length(id_case3)

					id_c5<-sample(id_c4,n.needed,prob=mu[id_c4])
					id_case<-union(id_case3,id_c5)
				}

				res.out[id_case,i]<-1
			}
		} else if(type.Resampling=="perturbation"){
			res.out<-matrix(rnorm(n1*n.Resampling,mean=0,sd=1),ncol=n.Resampling)
			res.out<-res.out * res
			stop("Error: Perturbation is no more provided!")
		} else {
			stop("Error: Wrong resampling method!")
		}
		res.out<-res.out - mu
	}

  	return(list(res=res, X1=X1,res.out=res.out,out_type="D",
	n.Resampling=n.Resampling, type.Resampling=type.Resampling,
	id_include=id_include, mu=mu,pi_1=pi_1))

}
