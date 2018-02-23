Get_RequiredSampleSize<-function (obj, Power=0.8){



	if(class(obj) == "SKAT_Power"){
		re<-Get_RequiredSampleSize.SKAT_Power(obj, Power)
	} else {
		re<-Get_RequiredSampleSize.numeric(obj, Power)
	}


	return(re)
}
