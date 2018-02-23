SKAT_Optimal_Get_Kertosis_Mixture<-function(df1, df2, v1, a1, a2){


	v2<-2*df2

	S4.1<-(12/df1 +3) * v1^2
 	S4.2<-(12/df2 +3) * v2^2

	#v1<-2*df1 + var.add


	S4<-a1^4*S4.1 + a2^4*S4.2 + 6 * a1^2 * a2^2 * v1 * v2
	S2<-a1^2*v1 + a2^2*v2

	K<-S4/(S2^2) - 3

	if(K < 0){
		K<-0.0001
	}


	#print(c(S4.1, S4.2, v1, v2, K, a1, a2, df1,df2))
	return(K)

}
