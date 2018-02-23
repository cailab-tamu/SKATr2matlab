call_Kernel_IBS_Weight<-function(Z,n,p,weights){

	#Kernel_IBS_Weight(int * Z, int * pn, int * pp, int *UseGivenWeight ,  double * weight, double * Kernel)
	given_weight = 1;
	if( is.null(weights)){
		weights = rep(0,p);
		given_weight = 0;
	}
	K<- matrix(rep(0,n*n),nrow = n, ncol = n)
	temp<-.C("Kernel_IBS_Weight",as.integer(as.vector(t(Z))),as.integer(n), as.integer(p),as.integer(given_weight),
	as.double(weights),as.double(as.vector(K)))[[6]]
	matrix(temp,nrow=n)
}
