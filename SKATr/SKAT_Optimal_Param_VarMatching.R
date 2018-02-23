SKAT_Optimal_Param_VarMatching<-function(Z1, r.all, p_all, res.moments, method="Other"){


	n<-dim(Z1)[1]
	p.m<-dim(Z1)[2]
	r.n<-length(r.all)

	z_mean<-rowMeans(Z1)
	Z_mean<-matrix(rep(z_mean,p.m),ncol=p.m,byrow=FALSE)
	cof1<-(t(z_mean) %*% Z1)[1,] / sum(z_mean^2)

	Z.item1<-Z_mean %*% diag(cof1)
	Z.item2<-Z1 - Z.item1

	# W3.2.t<-t(Z.item2) %*% Z.item2 follows mixture of chisq distribution
	# apply adjustment

	Q.sim = NULL
	if(!is.null(res.moments)){

		Q.Temp.res1 = t(cbind(res.moments))%*%Z.item2
  		Q.sim = rowSums(rbind(Q.Temp.res1^2))/2

	}

	type = "Other"
	if(method == "ECP"){
		type = "OnlySim"
	}

	re.param<-SKAT_Logistic_VarMatching_GetParam1(Z.item2, p_all, Q.sim, type)

	# W3.3 Term : variance of remaining ...
	W3.3.item<-sum((t(Z.item1) %*% Z.item1) * (t(Z.item2) %*% Z.item2)) * 4

	# W3.1 Term : tau1 * chisq_1
	tau<-rep(0,r.n)
	for(i in 1:r.n){
		r.corr<-r.all[i]
		term1<-p.m*r.corr + cof1^2 * (1-r.corr)
		tau[i]<-sum(term1) *  sum(z_mean^2)
	}

	out<-list(param=re.param, VarRemain=W3.3.item, tau=tau)
	return(out)
}
