SKAT_Logistic_VarMatching_GetParam1_OnlySim<-function(Z1, p_all, Q.sim){


	out.svd = Get_Lambda_U_From_Z(Z1)
	lambda<-out.svd$lambda

	muQ = sum(lambda)
	varQ.sim<-var(Q.sim)

	#print(c(varQ, varQ.sim))
	df.sim<-SKAT_Get_DF_Sim(Q.sim)

	# No adjustment
	c1<-rep(0,4)
	for(i in 1:4){
		c1[i]<-sum(lambda^i)
	}
	param<-Get_Liu_Params_Mod(c1)

	return(list(muQ = muQ, varQ = varQ.sim, df=df.sim, lambda.new=NULL, param.noadj = param))

}
