SKAT_Optimal_Integrate_Func_Liu<-function(x,pmin.q,param.m,r.all){

	#x<-1
	#print(length(x))
	#print(x)
	#X1<<-x
	#x<-X1

	n.r<-length(r.all)
	n.x<-length(x)

	temp1<-param.m$tau %x% t(x)

	temp<-(pmin.q - temp1)/(1-r.all)
	temp.min<-apply(temp,2,min)

	temp.q<-(temp.min - param.m$MuQ)/sqrt(param.m$VarQ)*sqrt(2*param.m$Df) + param.m$Df
	re<-pchisq(temp.q ,df=param.m$Df) * dchisq(x,df=1)
	return(re)

}
