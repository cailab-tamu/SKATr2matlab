KMTest.logistic.Linear = function(res, Z, X1, kernel, weights = NULL, pi_1, method,res.out,n.Resampling,r.corr){

  # Weighted Linear Kernel
  if (kernel == "linear.weighted") {
    Z = t(t(Z) * (weights))
  }

  # r.corr
  if(r.corr == 1){
  	Z<-cbind(rowSums(Z))
  } else if(r.corr > 0){

   	p.m<-dim(Z)[2]	
	R.M<-diag(rep(1-r.corr,p.m)) + matrix(rep(r.corr,p.m*p.m),ncol=p.m)
	L<-chol(R.M,pivot=TRUE)
	Z<- Z %*% t(L)
  }

  # Get temp
  Q.Temp = t(res)%*%Z
  Q = Q.Temp %*% t(Q.Temp)/2

  Q.res = NULL
  if(n.Resampling > 0){
  	Q.Temp.res = t(res.out)%*%Z
  	Q.res = rowSums(rbind(Q.Temp.res^2))/2
  }


  #gg = X1%*%solve(t(X1)%*%(X1 * pi_1))%*%t(X1 * pi_1)  ### Just a holder... not all that useful by itself
  #P0 = D-(gg * pi_1)      ### This is the P0 or P in Zhang and Lin
  # P0 = D-D%*%gg
  # P0 = D- D%*%X1%*%solve(t(X1)%*%(X1 * pi_1))%*%t(X1) %*% D



  #W = P0%*%K
  #W = K * pi_1 - (X1 *pi_1) %*%solve(t(X1)%*%(X1 * pi_1))%*% ( t(X1 * pi_1) %*% K)
  #muq  = sum(diag(W))/2   # this is the same as e-tilde

  # tr(W W) = tr(P0 K P0 K ) = tr ( Z^T P0 Z Z^T P0 Z ) = tr( P0 Z Z^T P0 Z Z^T )
  # tr(P0 K P0)
  # tr(A B) = tr(A * t(B))

  W.1 = t(Z) %*% (Z * pi_1) - (t(Z * pi_1) %*%X1)%*%solve(t(X1)%*%(X1 * pi_1))%*% (t(X1) %*% (Z * pi_1)) # t(Z) P0 Z


  if( method == "liu" ){
	out<-Get_Liu_PVal(Q, W.1, Q.res)
  } else if( method == "liu.mod" ){
	out<-Get_Liu_PVal.MOD(Q, W.1, Q.res)
  } else if( method == "davies" ){
	out<-Get_Davies_PVal(Q, W.1, Q.res)
  } else {
	stop("Invalid Method!")
  }


  re<-list(p.value = out$p.value, p.value.resampling = out$p.value.resampling, Test.Type = method, Q = Q,  Q.resampling = Q.res, param=out$param )
  return(re)
}
