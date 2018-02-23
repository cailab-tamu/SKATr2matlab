Get_Lambda_U_From_Z<-function(Z1){

	if(dim(Z1)[2]==1){
		Is.OnlyOne = TRUE
		lambda<-sum(Z1^2)
		U<-Z1/sqrt(lambda)
		return(list( lambda = lambda, U = cbind(U)))
	}

	try1<-try(svd(Z1, LINPACK = TRUE),silent = TRUE)
	if(class(try1) == "try-error"){
		# try LAPACK
		try1<-try(svd(Z1, LINPACK = FALSE),silent = TRUE)
	}

	if(class(try1) == "try-error"){
		stop("SVD error!");
	} else {
		out.svd = try1
	}

	lambda.org<-out.svd$d^2
	IDX<-which(lambda.org > mean(lambda.org)/100000)
	if(length(IDX) <= 1){
		Is.OnlyOne = TRUE
	}

	if(length(IDX) == 0){
		return(list(lambda=NULL, U=NULL))
	}
	return(list( lambda = lambda.org[IDX], U = cbind(out.svd$u[,IDX])))
}
