Get_MAF<-function(Z){

	is.missing<-which(Z == 9)
	Z[is.missing]<-NA

	maf<-colMeans(Z,na.rm = TRUE)/2
	return(maf)

}
