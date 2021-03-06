SKAT_Optimal_Get_Q<-function(Z1, res, r.all, n.Resampling, res.out, res.moments=NULL){

	n.r<-length(r.all)
	p.m<-dim(Z1)[2]

	Q.r<-rep(0,n.r)
	Q.r.res<-NULL
	Q.sim<-NULL

	temp<-t(res) %*% Z1
	for(i in 1:n.r){
		r.corr<-r.all[i]
		Q1<-(1-r.corr) * rowSums(temp^2)
		Q2<-r.corr * p.m^2 * rowMeans(temp)^2
		Q.r[i]<-Q1 + Q2
	}
	Q.r = Q.r /2
  	if(n.Resampling > 0){

		temp<-t(res.out) %*% Z1
		Q.r.res<-matrix(rep(0,n.Resampling *n.r),ncol=n.r)
		for(i in 1:n.r){
			r.corr<-r.all[i]
			Q1<-(1-r.corr) * rowSums(temp^2)
			Q2<-r.corr * p.m^2 * rowMeans(temp)^2
			Q.r.res[,i]<-Q1 + Q2
		}
		Q.r.res = Q.r.res/2
  	}

	if(!is.null(res.moments)){

		temp<-t(res.moments) %*% Z1
		n.moments<-dim(res.moments)[2]
		Q.sim<-matrix(rep(0,n.moments *n.r),ncol=n.r)
		for(i in 1:n.r){
			r.corr<-r.all[i]
			Q1<-(1-r.corr) * rowSums(temp^2)
			Q2<-r.corr * p.m^2 * rowMeans(temp)^2
			Q.sim[,i]<-Q1 + Q2
		}
		Q.sim = Q.sim/2

	}

	re<-list(Q.r=Q.r, Q.r.res=Q.r.res , Q.sim=Q.sim)
	return(re)


}
