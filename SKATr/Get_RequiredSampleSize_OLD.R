Get_RequiredSampleSize_OLD<-function(Power.Est, Power=0.8){

	N.Sample.ALL<-as.numeric(rownames(Power.Est))
	alpha<-as.numeric(colnames(Power.Est))

	re<-rep(0,length(alpha))
	for(i in 1:length(alpha)){
		temp1<-smooth.spline(Power.Est[,i],N.Sample.ALL)
		yy1<-predict(temp1,0.8)$y
		#print(yy1)
		re[i]<-ceiling(yy1 /10) *10
	}
	names(re)<-alpha
	return(re)
}
