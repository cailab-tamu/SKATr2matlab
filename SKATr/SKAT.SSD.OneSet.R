SKAT.SSD.OneSet = function(SSD.INFO, SetID, obj, ...){

	id1<-which(SSD.INFO$SetInfo$SetID == SetID)
	if(length(id1) == 0){
		MSG<-sprintf("Error: cannot find set id [%s] from SSD!", SetID)
		stop(MSG)
	}
	Set_Index<-SSD.INFO$SetInfo$SetIndex[id1]

	Z<-Get_Genotypes_SSD(SSD.INFO, Set_Index)
	re<-SKAT(Z, obj, ...)

	return(re)
}
