Get_B_Q<-function(Z, eta){

	N<-dim(Z)[1]
	m<-length(eta)
	mu<-eta

	Z1<- t(t(Z) - colMeans(Z))
	B1<- (t(Z1) %*%  mu )[,1] / N
	B<- B1 %*% t(B1)
	return(B)
}
