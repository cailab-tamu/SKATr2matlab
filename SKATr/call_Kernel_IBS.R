call_Kernel_IBS<-function(Z,n,p){

	#Kernel_IBS(double * Z, int * pn, int * pp, double * Kernel)
	K<- matrix(rep(0,n*n),nrow = n, ncol = n)
	temp<-.C("Kernel_IBS",as.integer(as.vector(t(Z))),as.integer(n), as.integer(p),as.double(as.vector(K)))[[4]]
	matrix(temp,nrow=n)
}
