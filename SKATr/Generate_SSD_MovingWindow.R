Generate_SSD_MovingWindow<-function(File.Bed, File.Bim, File.Fam, File.SSD, File.Info, WindowSize,Overlap){

	File.Bed<-normalizePath(File.Bed ,mustWork =FALSE)
	File.Bim<-normalizePath(File.Bim ,mustWork =FALSE)
	File.Fam<-normalizePath(File.Fam ,mustWork =FALSE)
	File.SSD<-normalizePath(File.SSD ,mustWork =FALSE)
	File.Info<-normalizePath(File.Info ,mustWork =FALSE)


	Check_File_Exists(File.Bed)
	Check_File_Exists(File.Bim)
	Check_File_Exists(File.Fam)


	err_code<-0

	temp<-.C("R_Generate_MWA_MovingWindow"
	, as.character(File.Bed), as.character(File.Bim), as.character(File.Fam)
	, as.character(File.SSD), as.integer(WindowSize), as.integer(Overlap), as.character(File.Info)
	, as.integer(err_code))

	Kill_SSD_MovingWindow()

	error_code<-temp[[8]]
	Print_Error_SSD(error_code)


	Read_File_Info_Head(File.Info)
	print("SSD and Info files are created!")

}
