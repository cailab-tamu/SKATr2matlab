Get_A_Q<-function(Z){

	N<-dim(Z)[1]
	Z1<- t(t(Z) - colMeans(Z))
	A<- t(Z1) %*% (Z1 ) / N
	return(A)

}
