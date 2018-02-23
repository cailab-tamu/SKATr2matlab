SKAT_Logistic_VarMatching_GetParam1<-function(Z1, p_all, Q.sim, type="Other"){


	if(type != "OnlySim"){

		try1<-try(Get_Lambda_U_From_Z(Z1),silent = TRUE)
		if(class(try1) == "try-error"){
			type="OnlySim"
		} else {
			out.svd = try1
			lambda<-out.svd$lambda
    			U<-out.svd$U
			param<-SKAT_Logistic_VarMatching_GetParam(lambda, U, p_all, Q.sim)
		}

	}

	if(type == "OnlySim"){
		param<-SKAT_Logistic_VarMatching_GetParam1_OnlySim(Z1, p_all, Q.sim)

	}


	return(param)

}
