SKAT_Logistic_VarMatching_GetParam <-function(lambda, U, p_all, Q.sim){

	# Var match
	re<-SKAT_Get_Cov_Param(lambda, p_all, U)

	# New Lambda
	lambda.new<- re$lambda.new

	# new parameters
	muQ<-re$muQ

	# new var
	varQ<-re$varQ

	# df
	s2 = sum(lambda.new^4) / sum(lambda.new^2)^2
	df<-1/s2

	if(!is.null(Q.sim)){
		df<-SKAT_Get_DF_Sim(Q.sim)
	}


	# No adjustment
	c1<-rep(0,4)
	for(i in 1:4){
		c1[i]<-sum(lambda^i)
	}
	param<-Get_Liu_Params_Mod(c1)

	return(list(muQ = muQ, varQ = varQ, df=df, lambda.new=lambda.new, param.noadj = param))

}
