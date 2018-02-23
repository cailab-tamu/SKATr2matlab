SKAT_Null_Model_MomentAdjust = function(formula, data=NULL, n.Resampling=0, type.Resampling="bootstrap", is_kurtosis_adj=TRUE, n.Resampling.kurtosis=10000){


	# check missing
	obj1<-model.frame(formula,na.action = na.omit,data)
	obj2<-model.frame(formula,na.action = na.pass,data)

	n<-dim(obj2)[1]
	n1<-dim(obj1)[1]
	id_include<-as.numeric(rownames(obj1))

	if(n - n1 > 0){
		MSG<-sprintf("%d  samples have either missing phenotype or missing covariates. They are excluded from the analysis!",n - n1)
		warning(MSG,call.=FALSE)
	}

	re1<-Get_SKAT_Residuals.logistic (formula, data, n.Resampling, type.Resampling, id_include )
	re2<-NULL

	if(is_kurtosis_adj == TRUE){
		re2<-Get_SKAT_Residuals.logistic (formula, data, n.Resampling.kurtosis, type.Resampling, id_include )
	}

	class(re1)<-"SKAT_NULL_Model"
	re<-list(re1=re1, re2=re2, is_kurtosis_adj= is_kurtosis_adj, type = "binary")

	class(re)<-"SKAT_NULL_Model_ADJ"
	return(re)

}
