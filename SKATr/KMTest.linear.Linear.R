KMTest.linear.Linear = function(res,Z,X1, kernel, weights, s2, method,res.out,n.Resampling,r.corr){

  n<-nrow(Z)
  # Weighted Linear Kernel
  if (kernel == "linear.weighted") {
    Z = t(t(Z) * (weights))
  }

  if(r.corr == 1){
  	Z<-cbind(rowSums(Z))
  } else if(r.corr > 0){

   	p.m<-dim(Z)[2]
	R.M<-diag(rep(1-r.corr,p.m)) + matrix(rep(r.corr,p.m*p.m),ncol=p.m)
	L<-chol(R.M,pivot=TRUE)
	Z<- Z %*% t(L)
  }


  # get Q
  Q.Temp = t(res)%*%Z
  Q = Q.Temp %*% t(Q.Temp)/s2/2

  Q.res = NULL
  if(n.Resampling > 0){
  	Q.Temp.res = t(res.out)%*%Z
  	Q.res = rowSums(rbind(Q.Temp.res^2))/s2/2
  }

  W.1 = t(Z) %*% Z - (t(Z) %*%X1)%*%solve(t(X1)%*%X1)%*% (t(X1) %*% Z ) # t(Z) P0 Z

  if( method == "liu" ){
	out<-Get_Liu_PVal(Q, W.1, Q.res)
  } else if( method == "liu.mod" ){
	out<-Get_Liu_PVal.MOD(Q, W.1, Q.res)
  } else if( method == "davies" ){
	out<-Get_Davies_PVal(Q, W.1, Q.res)
  } else {
	stop("Invalid Method!")
  }

  re<-list(p.value = out$p.value, p.value.resampling = out$p.value.resampling, Test.Type = method, Q = Q, param=out$param )
  return(re)

}
