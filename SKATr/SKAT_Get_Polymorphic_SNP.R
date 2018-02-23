SKAT_Get_Polymorphic_SNP<-function(Z){

	temp<-apply(Z,2,var)
	ID<-which(temp == 0)
	return(ID)
}
