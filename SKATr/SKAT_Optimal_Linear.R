SKAT_Optimal_Linear = function(res,Z,X1, kernel, weights = NULL, s2,method=NULL
, res.out=NULL, n.Resampling =0, r.all){

	# if r.all >=0.999 ,then r.all = 0.999. It is just for computation.
	IDX<-which(r.all >= 0.999)
	if(length(IDX) > 0){
		r.all[IDX]<-0.999
	}

	n<-dim(Z)[1]
	p.m<-dim(Z)[2]
	n.r<-length(r.all)

	if (kernel == "linear.weighted") {
		Z = t(t(Z) * (weights))
	}

  	#Get P0Z, wher P0 = diag(n) - X1%*%solve( t(X1)%*%X1)%*%t(X1)
	Z1<-Z - X1%*%solve( t(X1)%*%X1)%*%(t(X1) %*% Z)

	###########################################
	# Compute Q.r and Q.r.res
	##########################################
	out.Q<-SKAT_Optimal_Get_Q(Z, res, r.all, n.Resampling, res.out)
	Q.all<-rbind(out.Q$Q.r, out.Q$Q.r.res) / s2

	##################################################
	# Compute P-values
	#################################################

	out<-SKAT_Optimal_Get_Pvalue(Q.all, Z1 / sqrt(2), r.all, method)

	param<-list(p.val.each=NULL,q.val.each=NULL)
	param$p.val.each<-out$p.val.each[1,]
	param$q.val.each<-Q.all[1,]
	param$rho<-r.all
	param$minp<-min(param$p.val.each)

	id_temp<-which(param$p.val.each == min(param$p.val.each))
	id_temp1<-which(param$rho >= 0.999) # treat rho > 0.999 as 1
	if(length(id_temp1) > 0){
		param$rho[id_temp1] = 1
	}
	param$rho_est<-param$rho[id_temp]


	p.value<-out$p.value[1]

	p.value.resampling<-NULL
	if(n.Resampling > 0){
		p.value.resampling<-out$p.value[-1]
		#param$pval.each.resample<-out$p.val.each[-1]
	}

 	re<-list(p.value = p.value, p.value.resampling = p.value.resampling
	, Test.Type = method, Q = NA, param=param )

	return(re)

}
