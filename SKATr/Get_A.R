Get_A<-function(Z,eta, Dist,ratio){

	alpha<-log(ratio/(1-ratio))
	Z1<- t(t(Z) - colMeans(Z))
	m<-length(eta)
	V<-exp(eta + alpha) / ( 1 + exp(eta + alpha))^2
	A<- t(Z1) %*% (Z1 * V * Dist)

	return(A)

}
