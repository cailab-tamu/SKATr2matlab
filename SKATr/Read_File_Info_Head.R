Read_File_Info_Head<-function(File.Info, Is.Print=TRUE){

	Check_File_Exists(File.Info)

	info1<-read.table(File.Info, header=FALSE, nrows= 6, sep='\t')


	INFO<-list()
	INFO$WindowSize<-info1[1,1]
	INFO$OverlapSize<-info1[2,1]
	INFO$nSNPs<-info1[3,1]
	INFO$nSample<-info1[4,1]
	INFO$nDecodeSize<-info1[5,1]
	INFO$nSets<-info1[6,1]

	if(Is.Print == TRUE){
		Print_File_Info(INFO)
	}

	return(list(nSets=INFO$nSets))

}
