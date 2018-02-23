SKAT_PValue_Logistic_VarMatching<-function(Q, Z1, p_all, Q.sim, type="Other"){


	param<-SKAT_Logistic_VarMatching_GetParam1(Z1, p_all, Q.sim, type)

	# aSKAT pvalue
	Q.Norm<-(Q - param$muQ)/sqrt(param$varQ)
	Q.Norm1<-Q.Norm * sqrt(2*param$df) + param$df
	p.value<- 1-pchisq(Q.Norm1,  df = param$df, ncp=0)


	# SKAT pvalue
	param.noadj<-param$param.noadj
	Q.Norm<-(Q - param.noadj$muQ)/param.noadj$sigmaQ
	Q.Norm1<-Q.Norm * param.noadj$sigmaX + param.noadj$muX
	p.value.noadj<- 1-pchisq(Q.Norm1,  df = param.noadj$l,ncp=0)

	out<-list(p.value=p.value, p.value.noadj=p.value.noadj, param=param)

	return(out)

}
