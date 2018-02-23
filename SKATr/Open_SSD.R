Open_SSD<-function(File.SSD, File.Info){

	err_code<-0
	File.SSD<-normalizePath(File.SSD ,mustWork =FALSE)
	File.Info<-normalizePath(File.Info ,mustWork =FALSE)

	Check_File_Exists(File.SSD)
	Check_File_Exists(File.Info)

	if(get("SSD_FILE_OPEN.isOpen", envir=.GlobalEnv) == 1){
		Close_SSD();
	}

	# Read Info File
	INFO<-Read_File_Info(File.Info)

	# Read SSD File
	temp<-.C("R_Open_MWA", as.character(File.SSD), as.character(File.Info)
	, as.integer(err_code))

	error_code<-temp[[3]]
	Print_Error_SSD(error_code)


	Msg<-sprintf("Open the SSD file\n");
	cat(Msg)

	#SSD_FILE_OPEN.isOpen<<-1
	#SSD_FILE_OPEN.FileName<<-File.SSD

	assign("SSD_FILE_OPEN.isOpen", 1, envir=.GlobalEnv)
	assign("SSD_FILE_OPEN.FileName",File.SSD, envir=.GlobalEnv)


	return(INFO)

}
