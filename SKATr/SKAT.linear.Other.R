SKAT.linear.Other = function(res,Z,X1, kernel, weights = NULL, s2, method,res.out,n.Resampling){

  n<-nrow(Z)
  m = ncol(Z)
  if (class(kernel) == "matrix") {
    K = kernel
  } else {
    K = lskmTest.GetKernel(Z, kernel, weights,n,m)
  }

  Q = t(res)%*%K%*%res/(2*s2)

  Q.res = NULL
  if(n.Resampling > 0){
	Q.res<-rep(0,n.Resampling)
	for(i in 1:n.Resampling){
  		Q.res[i] = t(res.out[,i])%*%K%*%res.out[,i]/(2*s2)
  	}
  }


  W = K - X1%*%solve( t(X1)%*%X1)%*%( t(X1) %*% K)	# W = P0 K

  if(method == "davies"){
  	# P0_half = P0
	W1 = W - (W %*% X1) %*%solve( t(X1)%*%X1)%*% t(X1)
  }

  if( method == "liu" ){
	out<-Get_Liu_PVal(Q, W,Q.res)
  } else if( method == "liu.mod" ){
	out<-Get_Liu_PVal.MOD(Q, W,Q.res)
  } else if( method == "davies" ){
	out<-Get_Davies_PVal(Q, W1,Q.res)
  } else {
	stop("Invalid Method!")
  }

  re<-list(p.value = out$p.value, p.value.resampling = out$p.value.resampling, Test.Type = method, Q = Q, param=out$param )
  return(re)

}
