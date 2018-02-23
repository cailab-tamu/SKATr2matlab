Get_PValue<-function(K,Q){

	lambda<-Get_Lambda(K)
	#print(lambda)
	n1<-length(Q)

	p.val<-rep(0,n1)
	p.val.liu<-rep(0,n1)
	is_converge<-rep(0,n1)

	for(i in 1:n1){
		out<-SKAT_davies(Q[i],lambda,acc=10^(-6))

		p.val[i]<-out$Qq
		p.val.liu[i]<-SKAT_liu(Q[i],lambda)

		is_converge[i]<-1

		# check convergence
		if(length(lambda) == 1){
			p.val[i]<-p.val.liu[i]
		} else if(out$ifault != 0){
			is_converge[i]<-0
		}

		# check p-value
		if(p.val[i] > 1 || p.val[i] <= 0 ){
			is_converge[i]<-0
			p.val[i]<-p.val.liu[i]
		}
	}

	return(list(p.value=p.val, p.val.liu=p.val.liu, is_converge=is_converge))

}
