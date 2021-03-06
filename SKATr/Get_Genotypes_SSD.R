Get_Genotypes_SSD<-function(SSD_INFO, Set_Index){

	Is_MakeFile=0
	if(get("SSD_FILE_OPEN.isOpen", envir=.GlobalEnv) == 0){
		stop("SSD file is not opened. Please open it first!")
	}

	id1<-which(SSD_INFO$SetInfo$SetIndex == Set_Index)
	if(length(id1) == 0){
		MSG<-sprintf("Error: cannot find set index [%d] from SSD!\n", Set_Index)
		stop(MSG)
	}
	Set_Index<-SSD_INFO$SetInfo$SetIndex[id1]

	err_code<-0
	N.SNP<-SSD_INFO$SetInfo$SetSize[id1]
	N.Sample<-SSD_INFO$nSample
	size<-N.SNP * N.Sample

	Z<-rep(9,size)


	temp<-.C("R_Get_Genotypes",as.integer(Set_Index),as.integer(Z),as.integer(size)
	,as.integer(Is_MakeFile), as.integer(err_code))

	error_code<-temp[[5]]
	Print_Error_SSD(error_code)


	Z.out.t<-matrix(temp[[2]],byrow=TRUE, nrow=N.SNP)
	return(t(Z.out.t))

}
