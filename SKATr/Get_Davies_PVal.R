Get_Davies_PVal<-function(Q, W, Q.resampling = NULL){

	K<-W/2

	Q.all<-c(Q,Q.resampling)

	re<-Get_PValue(K,Q.all)
	param<-list()
	param$liu_pval<-re$p.val.liu[1]
	param$Is_Converged<-re$is_converge[1]


	p.value.resampling = NULL
	if(length(Q.resampling) > 0){
		p.value.resampling<-re$p.value[-1]
		param$liu_pval.resampling<-re$p.val.liu[-1]
		param$Is_Converged.resampling<-re$is_converge[-1]

	}


	re<-list(p.value = re$p.value[1], param=param,p.value.resampling = p.value.resampling )
	return(re)
}
