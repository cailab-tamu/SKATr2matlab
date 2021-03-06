Read_Plink_FAM<-function(Filename, Is.binary=TRUE, flag1=0){

	Check_File_Exists(Filename)
	Fam.Obj<-read.table(Filename, header=FALSE)
	colnames(Fam.Obj)<-c("FID","IID","PID","MID", "Sex", "Phenotype")

	id.missing<-which(Fam.Obj$Phenotype == -9)
	if(length(id.missing) > 0){
		Fam.Obj$Phenotype[id.missing]<-NA
	}

	if(Is.binary && flag1 == 0){

		id_0<-which(Fam.Obj$Phenotype == 0)
		id_1<-which(Fam.Obj$Phenotype == 1)
		id_2<-which(Fam.Obj$Phenotype == 2)

		if(length(id_0)> 0){
			Fam.Obj$Phenotype[id_0] <-NA
		}
		if(length(id_1)> 0){
			Fam.Obj$Phenotype[id_0] <-0
		}
		if(length(id_2)> 0){
			Fam.Obj$Phenotype[id_0] <-1
		}

	}

	return(Fam.Obj)

}
