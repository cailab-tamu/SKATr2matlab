Get_CausalSNPs<-function(MAF, Causal.Ratio, Causal.MAF.Cutoff){

	IDX<-which(MAF < Causal.MAF.Cutoff)
	N.causal<-round(Causal.Ratio * length(IDX))
	#print(N.causal)
	#print(Causal.Ratio)
	#print(length(IDX))
	re<-sort(sample(IDX,N.causal))
	return(re)
}
