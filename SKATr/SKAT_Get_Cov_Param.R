SKAT_Get_Cov_Param<-function(lambda,p_all,U){

	#p_all<-obj$mu
	#U<-out$U
	#lambda<-out$lambda

	p.m<-length(lambda)
	m4<-p_all*(1-p_all)*(3*p_all^2-3*p_all +1) / (p_all*(1-p_all))^2

	zeta<-rep(0,p.m)
	var_i<-rep(0,p.m)
	varQ<-0

	for(i in 1:p.m){
		temp.M1<-sum(U[,i]^2)^2 - sum(U[,i]^4)
		zeta[i]<-sum(m4 * U[,i]^4) + 3* temp.M1 # because ( \sum .)^4, not ^2
		var_i[i]<-zeta[i] - 1
	}

	if(p.m == 1){
		Cov_Mat<-matrix(zeta* lambda^2, ncol=1,nrow=1)
	} else if(p.m > 1){

		Cov_Mat<-diag(zeta* lambda^2)
		for(i in 1:(p.m-1)){
			for(j in (i+1):p.m){
				Cov_Mat[i,j]<-SKAT_Get_Var_Elements(m4,p_all,U[,i],U[,j])
				Cov_Mat[i,j]<-Cov_Mat[i,j]* lambda[i]* lambda[j]
			}
		}
	}

	Cov_Mat<-Cov_Mat + t(Cov_Mat)
	diag(Cov_Mat)<-diag(Cov_Mat)/2

	varQ<-sum(Cov_Mat) - sum(lambda)^2
	muQ=sum(lambda)
	lambda.new<-lambda * sqrt(var_i)/sqrt(2)
	return(list(zeta=zeta, var_i=var_i, varQ = varQ, muQ=muQ, lambda.new=lambda.new))

}
