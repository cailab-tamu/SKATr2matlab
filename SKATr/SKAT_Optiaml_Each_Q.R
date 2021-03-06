SKAT_Optiaml_Each_Q<-function(param.m, Q.all, r.all, lambda.all){

	n.r<-length(r.all)
	c1<-rep(0,4)
	n.q<-dim(Q.all)[1]

	pval<-matrix(rep(0,n.r*n.q),ncol=n.r)
	pmin.q<-matrix(rep(0,n.r*n.q),ncol=n.r)
	param.mat<-NULL

	for(i in 1:n.r){
		Q<-Q.all[,i]
		r.corr<-r.all[i]
		lambda.temp<-lambda.all[[i]]
		c1[1]<-sum(lambda.temp)
		c1[2]<-sum(lambda.temp^2)
		c1[3]<-sum(lambda.temp^3)
		c1[4]<-sum(lambda.temp^4)
		param.temp<-Get_Liu_Params_Mod(c1)

		muQ<-param.temp$muQ
		varQ<-param.temp$sigmaQ^2
		df<-param.temp$l

		# get pvalue
		Q.Norm<-(Q - muQ)/sqrt(varQ) * sqrt(2*df) + df
		pval[,i]<- 1-pchisq(Q.Norm,  df = df)

		param.mat<-rbind(param.mat,c(muQ,varQ,df))
	}

	pmin<-apply(pval,1,min)
	for(i in 1:n.r){

		muQ<-param.mat[i,1]
		varQ<-param.mat[i,2]
		df<-param.mat[i,3]

		q.org<-qchisq(1-pmin,df=df)
		q.q<-(q.org - df)/sqrt(2*df) *sqrt(varQ) + muQ
		pmin.q[,i]<-q.q

	}

	out<-list(pmin=pmin,pval=pval,pmin.q=pmin.q)
	return(out)

}
