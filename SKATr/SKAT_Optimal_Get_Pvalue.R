SKAT_Optimal_Get_Pvalue<-function(Q.all, Z1, r.all, method){

	n.r<-length(r.all)
	n.q<-dim(Q.all)[1]
	p.m<-dim(Z1)[2]

	lambda.all<-list()
	for(i in 1:n.r){
		r.corr<-r.all[i]
		R.M<-diag(rep(1-r.corr,p.m)) + matrix(rep(r.corr,p.m*p.m),ncol=p.m)
		L<-chol(R.M,pivot=TRUE)
		Z2<- Z1 %*% t(L)
		K1<-t(Z2) %*% Z2

		lambda.all[[i]]<-Get_Lambda(K1)

	}

	# Get Mixture param
	param.m<-SKAT_Optimal_Param(Z1,r.all)
	Each_Info<-SKAT_Optiaml_Each_Q(param.m, Q.all, r.all, lambda.all)
	pmin.q<-Each_Info$pmin.q
	pval<-rep(0,n.q)

	if(method == "davies" || method=="optimal"){

		for(i in 1:n.q){
			pval[i]<-SKAT_Optimal_PValue_Davies(pmin.q[i,],param.m,r.all)
		}


	} else if(method =="liu" || method =="liu.mod"){

		for(i in 1:n.q){
			pval[i]<-SKAT_Optimal_PValue_Liu(pmin.q[i,],param.m,r.all)
		}

	} else {
		stop("Invalid Method!")
	}
	return(list(p.value=pval,p.val.each=Each_Info$pval))

}
