Read_File_Info<-function(File.Info){

	Check_File_Exists(File.Info)

	info1<-read.table(File.Info, header=FALSE, nrows= 6, sep='\t')
	info2<-read.table(File.Info, header=FALSE, skip=7, sep='\t', stringsAsFactors=FALSE,comment.char = "")

	INFO<-list()
	INFO$WindowSize<-info1[1,1]
	INFO$OverlapSize<-info1[2,1]
	INFO$nSNPs<-info1[3,1]
	INFO$nSample<-info1[4,1]
	INFO$nDecodeSize<-info1[5,1]
	INFO$nSets<-info1[6,1]
	INFO$SetInfo<-data.frame(SetIndex= info2[,1], SetID = as.character(info2[,3])
	, SetSize = info2[,4], Offset = info2[,2],stringsAsFactors=FALSE)

	Print_File_Info(INFO)

	return(INFO)


}
