Check_ID_Length<-function(FileName){


	SSD.Info<-try(read.table(FileName, header=FALSE, stringsAsFactors=FALSE), silent=TRUE)
	if(class(SSD.Info)=="try-error"){
		stop("Error in SetID file!")
	}

	n1<-length(which(nchar(SSD.Info[,1]) > 25))
	n2<-length(which(nchar(SSD.Info[,2]) > 25))

	if(n1 > 0){
		stop("Some SetIDs have more than 25 characters!")
	}
	if(n2 > 0){
		stop("Some SNP_IDs have more than 25 characters!")
	}

	nSets<-length(unique(SSD.Info[,1]))
	return(list(nSets=nSets))
}
