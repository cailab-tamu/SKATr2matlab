Get_Matrix_Square.1<-function(A){

	out<-eigen(A,symmetric=TRUE)
	ID1<-which(out$values > 0)
	if(length(ID1)== 0){
		stop("Error to obtain matrix square!")
	}
	out1<-t(out$vectors[,ID1]) * sqrt(out$values[ID1])
	return(out1)
}
