SKAT_Optimal_Integrate_Func_VarMatching<-function(x, pmin.q, muQ, varQ, df, tau, r.all){


	n.r<-length(r.all)
	n.x<-length(x)

	temp1<-tau %x% t(x)

	temp<-(pmin.q - temp1)/(1-r.all)
	temp.min<-apply(temp,2,min)

	temp.q<-(temp.min - muQ)/sqrt(varQ)*sqrt(2*df) + df
	re<-pchisq(temp.q ,df=df) * dchisq(x,df=1)

	#df.x<-1
	#x.norm<-(x -1)/sqrt(2) * sqrt(2*df.x) + df.x
	#re<-pchisq(temp.q ,df=df) * dchisq(x.norm,df=df.x)
	return(re)

}
