Dist_Case_Control<-function(eta,Beta0, ratio_case){

	temp1<-Dist_Case(eta, Beta0) *ratio_case
	temp2<-Dist_Control(eta, Beta0)*(1-ratio_case)

	temp1 + temp2
}
