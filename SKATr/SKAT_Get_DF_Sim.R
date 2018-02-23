SKAT_Get_DF_Sim<-function(Q.sim){


	s2.sim<-SKAT_GET_kurtosis(Q.sim)
	df.sim<-12/s2.sim

	if(s2.sim <= 0){

		df.sim=100000
	} else if(df.sim < 0.01 ){
		s1.sim<-SKAT_GET_skewness(Q.sim)
		df.sim<-8/s1.sim^2
	}

	return(df.sim)
}
