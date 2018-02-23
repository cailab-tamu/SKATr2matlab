Get_Lambda<-function(K){

	out.s<-eigen(K,symmetric=TRUE, only.values = TRUE)
	#print(out.s$values)

	#out.s1<-eigen(K,symmetric=TRUE)
	#print(out.s1$values)

	lambda1<-out.s$values
	IDX1<-which(lambda1 >= 0)

	# eigenvalue bigger than sum(eigenvalues)/1000
	IDX2<-which(lambda1 > mean(lambda1[IDX1])/100000)

	if(length(IDX2) == 0){
		stop("No Eigenvalue is bigger than 0!!")
	}
	lambda<-lambda1[IDX2]
	lambda

}
