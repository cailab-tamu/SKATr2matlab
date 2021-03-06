Generate_SSD_SetID<-function(File.Bed, File.Bim, File.Fam, File.SetID, File.SSD, File.Info){


	re = Generate_SSD_SetID_Work(File.Bed, File.Bim, File.Fam, File.SetID, File.SSD, File.Info)

	if(re == -1){
		MSG1<-sprintf("Warning: SSD file has more SNP sets then SetID file.It happens when SNPs in sets are not contiguous!\n")
		MSG2<-sprintf("SKAT generates a temporary SetID file with contiguous SNP sets. However the order of SNP sets will be different from the order of SNP sets in the original SetID file! \n\n")
		cat(MSG1)
		cat(MSG2)

		File.SetID.temp<-Create_Temporaly_SetID(File.SetID)
		re = Generate_SSD_SetID_Work(File.Bed, File.Bim, File.Fam, File.SetID.temp, File.SSD, File.Info)
		if(re == -1){
			stop("Keep having more SetIDs in the SSD file!")
		}
	}

	Read_File_Info_Head(File.Info, Is.Print=TRUE)
	print("SSD and Info files are created!")

}
