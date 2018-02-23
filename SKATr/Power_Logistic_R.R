Power_Logistic_R<-function(Haplotypes=NULL, SNP.Location=NULL, SubRegion.Length=-1, Prevalence=0.01, Case.Prop=0.5, Causal.Percent=5, Causal.MAF.Cutoff=0.03, alpha =c(0.01,10^(-3),10^(-6)),N.Sample.ALL = 500 * (1:10), Weight.Param=c(1,25),N.Sim=100, OR.Type = "Log", MaxOR=5, Negative.Percent=0,r.corr=0){

	if(is.null(Haplotypes)){

		MSG_SKAT_Example()
		data(SKAT.haplotypes)

		Haplotypes<-SKAT.haplotypes$Haplotype
		SNP.Location<-SKAT.haplotypes$SNPInfo$CHROM_POS

	}
	if(is.null(SNP.Location)){
		stop("Error : SNP.Location is NULL")
	}

	Marker.MAF.ALL<-colMeans(Haplotypes)


	# approximated Beta0
	Beta0<-log(Prevalence/(1-Prevalence))

	#####################################
	# 	Compute Power
	######################################
	OUT.ALL<-NULL
	n1<-dim(Haplotypes)[1]

	for(i in 1:N.Sim){

		IDX.Marker<-Get_RandomRegion(SNP.Location,SubRegion.Length)
		if(n1 >= 5000){
			H1<-sample(1:n1,replace=FALSE)
			H2<-sample(1:n1,replace=FALSE)
		} else {
			H1<-sample(1:10000,replace=TRUE)
			H2<-sample(1:10000,replace=TRUE)
			H1[1:n1]<-sample(1:n1,replace=FALSE)
			H2[1:n1]<-sample(1:n1,replace=FALSE)
		}


		X1<-Haplotypes[H1,IDX.Marker] + Haplotypes[H2,IDX.Marker]
		Marker.MAF<-Marker.MAF.ALL[IDX.Marker]
		#print(Marker.MAF)

		Causal.Idx<-Get_CausalSNPs(Marker.MAF,  Causal.Percent/100, Causal.MAF.Cutoff)
		Marker.Causal.MAF<-Marker.MAF[Causal.Idx]
		#print(length(Causal.Idx))
		#print(Marker.Causal.MAF)
		Beta = Get_Beta(OR.Type, Marker.Causal.MAF, log(MaxOR),Negative.Percent/100)

		#print(Marker.Causal.MAF)
		#print(Beta)

		Causal.Idx1<-IDX.Marker[Causal.Idx]

		# Seunggeun Change
		#eta<-(Haplotypes[,Causal.Idx1] %*% Beta)[,1] - (t(Marker.Causal.MAF *2)  %*% Beta)[1,1]
		#eta1<-eta[H1] + eta[H2]
		eta1<-(X1[,Causal.Idx] %*% Beta)[,1] - (t(Marker.Causal.MAF *2)  %*% Beta)[1,1]

		#print(Beta)
		#####################################
		#
		#	Power

		OUT<-Get_Power_Logistic_R(X1,eta1,Beta0, Case.Prop, alpha,N.Sample.ALL,Weight.Param,r.corr)
		if(i==1){
			OUT.ALL<-OUT/N.Sim
		} else {
			OUT.ALL<-OUT.ALL + OUT/N.Sim
		}
		if(floor(i/10) * 10 == i){
			msg<-sprintf("%d/%d",i,N.Sim)
			print(msg)
		}
	}
	re<-list(Power = OUT.ALL)
	class(re)<-"SKAT_Power"

	return(re)
}
