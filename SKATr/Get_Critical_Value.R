Get_Critical_Value<-function(c1,alpha){


	param.null<-Get_Liu_Params_Mod(c1)
	#param.null<-Get_Liu_Params(c1)
	q.crit = (qchisq(1-alpha
	, df = param.null$l, ncp =param.null$d)
	-param.null$muX)*(param.null$sigmaQ/param.null$sigmaX) + param.null$muQ

	return(list(q.crit=q.crit,c.all=c1,param.null=param.null))

}
