Print_File_Info<-function(INFO){

	MSG<-sprintf("%d Samples, %d Sets, %d Total SNPs\n",INFO$nSample, INFO$nSets, INFO$nSNPs)
	cat(MSG)

}
