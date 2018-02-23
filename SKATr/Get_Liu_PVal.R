Get_Liu_PVal<-function(Q, W, Q.resampling = NULL){


	Q.all<-c(Q,Q.resampling)

	A1<-W/2
	A2<-A1 %*% A1

	c1<-rep(0,4)
	c1[1]<-sum(diag(A1))
	c1[2]<-sum(diag(A2))
	c1[3]<-sum(A1*t(A2))
	c1[4]<-sum(A2*t(A2))
	param<-Get_Liu_Params(c1)

	Q.Norm<-(Q.all - param$muQ)/param$sigmaQ
	Q.Norm1<-Q.Norm * param$sigmaX + param$muX
	p.value<- 1-pchisq(Q.Norm1,  df = param$l,ncp=param$d)

	p.value.resampling = NULL
	if(length(Q.resampling) > 0){
		p.value.resampling<-p.value[-1]
	}

	re<-list(p.value = p.value[1], param=param, p.value.resampling = p.value.resampling )
	return(re)
}
