Get_RandomRegion<-function(SNP.Dist,SubRegion.Length){

	if(SubRegion.Length < 0){
		return(1:length(SNP.Dist))
	}
	total.first<-min(SNP.Dist)
	total.last<-max(SNP.Dist)
	total.length<-total.last -total.first

	Region.Start<-runif(1) * (total.length - SubRegion.Length)  + total.first
	Region.End<-Region.Start + SubRegion.Length

	#print(c(Region.Start,Region.End,total.first,total.last))

	Marker.Idx1<-which(SNP.Dist >= Region.Start)
	Marker.Idx2<-which(SNP.Dist <= Region.End)
	IDX<-sort(intersect(Marker.Idx1,Marker.Idx2))

	return(IDX)

}
