Get_Beta <-function(Type, MAF, MaxValue,Sign=0){

	n<-length(MAF)
	re<-rep(0,n)
	IDX<-which(MAF > 0)
	if(Type == "Log"){
		re[IDX]<-abs(log10(MAF[IDX])) /4 * MaxValue
	} else if (Type == "Fixed") {
		re[IDX]<-MaxValue
	}

    	if(Sign > 0){
      		#temp.n<-round(n * Sign)
		temp.n<-floor(n * Sign)
		if(temp.n > 0){
      			temp.idx<-sample(1:n, temp.n)
      			re[temp.idx]<--re[temp.idx]
		}
    	}
	return(re)

}
