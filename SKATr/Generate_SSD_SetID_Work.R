Generate_SSD_SetID_Work<-function(File.Bed, File.Bim, File.Fam, File.SetID, File.SSD, File.Info){


	File.Bed<-normalizePath(File.Bed ,mustWork =FALSE)
	File.Bim<-normalizePath(File.Bim ,mustWork =FALSE)
	File.Fam<-normalizePath(File.Fam ,mustWork =FALSE)
	File.SetID<-normalizePath(File.SetID ,mustWork =FALSE)
	File.SSD<-normalizePath(File.SSD ,mustWork =FALSE)
	File.Info<-normalizePath(File.Info ,mustWork =FALSE)


	Check_File_Exists(File.Bed)
	Check_File_Exists(File.Bim)
	Check_File_Exists(File.Fam)
	Check_File_Exists(File.SetID)

	nSet1 = Check_ID_Length(File.SetID)$nSet

	err_code<-0


	temp<-.C("R_Generate_MWA_SetID_File"
	, as.character(File.Bed), as.character(File.Bim), as.character(File.Fam)
	, as.character(File.SetID), as.character(File.SSD), as.character(File.Info)
	, as.integer(err_code))


	error_code<-temp[[7]]
	Print_Error_SSD(error_code)

        # Check SetID_LOG file
	Kill_SSD_SetID()
	Check_SetID_LOG(File.SSD)

	nSet2 = Read_File_Info_Head(File.Info, Is.Print=FALSE)$nSet

	re=1;
	if(nSet1 < nSet2){
		re=-1;	# re=-1 not matching numbers
	}

	return(re)
}
