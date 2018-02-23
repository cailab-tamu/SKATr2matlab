SKAT.SSD.OneSet_SetIndex = function(SSD.INFO, SetIndex, obj, ...){

	id1<-which(SSD.INFO$SetInfo$SetIndex == SetIndex)
	if(length(id1) == 0){
		MSG<-sprintf("Error: cannot find set index [%d] from SSD!", SetIndex)
		stop(MSG)
	}
	SetID<-SSD.INFO$SetInfo$SetID[id1]


	Z<-Get_Genotypes_SSD(SSD.INFO, SetIndex)
	re<-SKAT(Z, obj, ...)
	return(re)
}
