SKAT.logistic.Other = function(res, Z, X1, kernel , weights = NULL, pi_1, method,res.out,n.Resampling){

  n = nrow(Z)
  m = ncol(Z)

  # If m >> p and ( linear or linear.weight) kernel than call
  # Linear function

  if (class(kernel) == "matrix") {
    K = kernel
  } else {
    K = lskmTest.GetKernel(Z, kernel, weights,n,m)
  }


  Q = t(res)%*%K%*%res/2
  Q.res = NULL
  if(n.Resampling > 0){
	Q.res<-rep(0,n.Resampling)
	for(i in 1:n.Resampling){
  		Q.res[i] = t(res.out[,i])%*%K%*%res.out[,i]/2
  	}
  }

  D  = diag(pi_1)
  gg = X1%*%solve(t(X1)%*%(X1 * pi_1))%*%t(X1 * pi_1)  ### Just a holder... not all that useful by itself
  P0 = D-(gg * pi_1)      ### This is the P0 or P in Zhang and Lin
  # P0 = D-D%*%gg

  if(method == "davies"){
  	P0_half = Get_Matrix_Square.1(P0)
	#print(dim(P0_half))
	W1 = P0_half %*% K %*% t(P0_half)
  } else {
	#W    = P0%*%K
  	W = K * pi_1 - (X1 *pi_1) %*%solve(t(X1)%*%(X1 * pi_1))%*% ( t(X1 * pi_1) %*% K)
  }

  if( method == "liu" ){
	out<-Get_Liu_PVal(Q, W, Q.res)
  } else if( method == "liu.mod" ){
	out<-Get_Liu_PVal.MOD(Q, W, Q.res)
  } else if( method == "davies" ){
	out<-Get_Davies_PVal(Q, W1, Q.res)
  } else {
	stop("Invalid Method!")
  }

  re<-list(p.value = out$p.value, p.value.resampling = out$p.value.resampling, Test.Type = method, Q = Q, param=out$param )
  return(re)

}
