Create_Temporaly_SetID<-function(FileName){


	FileName.out<-sprintf("%s.temp",FileName)
	SSD.Info<-try(read.table(FileName, header=FALSE, stringsAsFactors=FALSE), silent=TRUE)
	if(class(SSD.Info)=="try-error"){
		stop("Error in SepID file!")
	}

	ord1<-order(SSD.Info[,1])
	SSD.Info1<-SSD.Info[ord1,]

	write.table(SSD.Info1, file=FileName.out, quote=FALSE, row.names=FALSE, col.names=FALSE)

	return(FileName.out)
}
