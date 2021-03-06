Check_SetID_LOG<-function(File.SSD){

	File.LOG<-sprintf("%s_LOG.txt",File.SSD)
	if(!file.exists(File.LOG)){
		MSG<-sprintf("Error %s file did not generated!\n",File.LOG)
		stop(MSG)
	}

	temp<-read.delim(File.LOG, header = FALSE, sep = "\n",comment.char = "")
	N.Miss<-dim(temp)[1] -1

	if(N.Miss > 0){
		MSG1<-sprintf("Warning: %d SNPs in the SetID file were not found in Bim file!\n",N.Miss)
		MSG2<-sprintf("Please check %s file!\n",File.LOG)

		cat(MSG1)
		cat(MSG2)
	}

}
