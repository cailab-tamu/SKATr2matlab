Impute<-function(Z, impute.method){

	p<-dim(Z)[2]

	if(impute.method =="random"){
		for(i in 1:p){
			IDX<-which(is.na(Z[,i]))
			if(length(IDX) > 0){
				maf1<-mean(Z[-IDX,i])/2
				Z[IDX,i]<-rbinom(length(IDX),2,maf1)
			}
		}
	} else if(impute.method =="fixed"){
		for(i in 1:p){
			IDX<-which(is.na(Z[,i]))
			if(length(IDX) > 0){
				maf1<-mean(Z[-IDX,i])/2
				Z[IDX,i]<-2 * maf1
			}
		}
	} else {
		stop("Error: Imputation method shoud be either \"fixed\" or \"random\"! ")
	}

	return(Z)
}
