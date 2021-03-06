lskmTest.GetKernel = function(Z, kernel, weights,n,m){

	if (kernel == "linear") {
      		K = Z%*%t(Z)/m
    	}
    	if (kernel == "linear.weighted") {
     		#K = matrix(nrow = n, ncol = n)
      		if (is.null(weights)) {
        		qs = apply(Z, 2, mean)/(2)
        		weights = 1/sqrt(qs)
      		}
      		Z1 = t(t(Z)*(weights))
      		K = Z1%*%t(Z1)/m
    	}
    	if (kernel == "quadratic") {
      		K = (Z%*%t(Z)+1)**2
    	}


	if (kernel == "IBS") {
      		K = call_Kernel_IBS(Z,n,m)
    	}
    	if (kernel == "IBS.weighted") {
      		K = call_Kernel_IBS_Weight(Z,n,m,weights)
    	}
  	if (kernel == "2wayIX") {
      		K = call_Kernel_2wayIX(Z,n,m)
    	}
   	if (kernel == "IBS.weighted_OLD") {
      		#K = matrix(nrow = n, ncol = n)
      		if (is.null(weights)) {
        		qs = apply(Z, 2, mean)/(2)
        		weights = 1/sqrt(qs)
      		}
      		K1 = matrix(nrow =n, ncol = n)
      		for (i in 1:n) {
        		K1[i,] = apply(abs(t(Z)-Z[i,])*weights,2, sum)
      		}
      		K= 1-(K1)/(2*sum(weights))
    	}

    	if (kernel == "IBS_OLD") {
      		K1=matrix(nrow=n,ncol=n)
      		for (i in 1:n) {
        		K1[i,] = apply(abs(t(Z)-Z[i,]),2, sum)
      		}
      		K = (2*m-K1)/(2*m)
    	}
   	if (kernel == "2wayIX_OLD") {
      		K = 1+Z%*%t(Z)
      		N1=  matrix(nrow = n, ncol = n)
      		for (i in 1:n){
        		for (j in i:n){
	    			N1[j,i] = N1[i,j] = K1_Help(Z[i,], Z[j,])
	  		}
      		}
      		K = K+N1
    	}
	return(K)

}
