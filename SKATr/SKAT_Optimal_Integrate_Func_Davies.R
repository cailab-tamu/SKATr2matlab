SKAT_Optimal_Integrate_Func_Davies<-function(x,pmin.q,param.m,r.all){

	n.r<-length(r.all)
	n.x<-length(x)

	temp1<-param.m$tau %x% t(x)

	temp<-(pmin.q - temp1)/(1-r.all)
	temp.min<-apply(temp,2,min)

	re<-rep(0,length(x))
	for(i in 1:length(x)){
		#a1<<-temp.min[i]
		min1<-temp.min[i]
		if(min1 > sum(param.m$lambda) * 10^4){
			temp<-0
		} else {
			min1.temp<- min1 - param.m$MuQ
			sd1<-sqrt(param.m$VarQ - param.m$VarRemain)/sqrt(param.m$VarQ)
			min1.st<-min1.temp *sd1 + param.m$MuQ

			dav.re<-SKAT_davies(min1.st,param.m$lambda,acc=10^(-6))
			temp<-dav.re$Qq
			if(dav.re$ifault != 0){
				stop("dav.re$ifault is not 0")
			}
		}
		if(temp > 1){
			temp=1
		}
		#lambda.record<<-param.m$lambda
		#print(c(min1,temp,dav.re$ifault,sum(param.m$lambda)))
		re[i]<-(1-temp) * dchisq(x[i],df=1)
	}
	return(re)

}
