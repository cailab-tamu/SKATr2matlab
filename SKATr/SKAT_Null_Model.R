SKAT_Null_Model = function(formula, data=NULL, out_type="C", n.Resampling=0, type.Resampling="bootstrap", Adjustment=TRUE){

	SKAT_MAIN_Check_OutType(out_type)


	# check missing
	obj1<-model.frame(y.c ~ X,na.action = na.omit)
	obj2<-model.frame(y.c ~ X,na.action = na.pass)

	n<-dim(obj2)[1]
	n1<-dim(obj1)[1]
	id_include<-SKAT_Null_Model_Get_Includes(obj1,obj2)

	# Check whether n < 1000 and out_type="D", apply the adjustment
	# if No_Adjustment = FALSE
	if(n< 2000 && out_type=="D" && Adjustment){
		MSG<-sprintf("Sample size = %d, which is < 2000. The small sample adjustment is applied!\n",n )
		cat(MSG)
		n.Resampling.kurtosis=10000
		#if(n > 1000){
		#	n.Resampling.kurtosis = floor(10000 - (n-1000) * 5)
		#}
		#if(n.Resampling.kurtosis < 5000){
		#	n.Resampling.kurtosis = 5000
		#}


		re<-SKAT_Null_Model_MomentAdjust(formula, data, n.Resampling, type.Resampling="bootstrap", is_kurtosis_adj=TRUE, n.Resampling.kurtosis=n.Resampling.kurtosis)
		return(re)
	}


	if(n - n1 > 0){
		MSG<-sprintf("%d  samples have either missing phenotype or missing covariates. They are excluded from the analysis!",n - n1)
		warning(MSG,call.=FALSE)
	}

	if(out_type=="C"){
		re<-Get_SKAT_Residuals.linear(formula, data, n.Resampling, type.Resampling, id_include )
	} else {
		re<-Get_SKAT_Residuals.logistic (formula, data, n.Resampling, type.Resampling, id_include )
	}

	class(re)<-"SKAT_NULL_Model"
	return(re)

}
