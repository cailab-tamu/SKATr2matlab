Get_B<-function(Z, eta, Dist,ratio){

	m<-length(eta)
	alpha<-log(ratio/(1-ratio))
	mu<-( exp(eta + alpha) / (1 + exp(eta + alpha)) - ratio)
	#mu<- exp(eta) / (1 + exp(eta))
	Z1<- t(t(Z) - colMeans(Z))

	B1<- (t(Z1) %*% ( mu * Dist))[,1]
	B<- B1 %*% t(B1)


	return(B)
}
