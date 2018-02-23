Close_SSD<-function(){

	if(get("SSD_FILE_OPEN.isOpen", envir=.GlobalEnv) == 1){
		temp<-.C("R_Close_MWA")
		Msg<-sprintf("Close the opened SSD file: %s\n"
		,get("SSD_FILE_OPEN.FileName", envir=.GlobalEnv));
		cat(Msg)
		assign("SSD_FILE_OPEN.isOpen", 0, envir=.GlobalEnv);
	} else{
		Msg<-sprintf("No opened SSD file!\n");
		cat(Msg)
	}
}
