Get_RequiredSampleSize.numeric<-function(Power.Est, Power=0.8){

	N.Sample.ALL<-as.numeric(rownames(Power.Est))
	alpha<-as.numeric(colnames(Power.Est))

	re<-list()
	for(i in 1:length(alpha)){


		temp<-which(Power.Est[,i] > Power)
		if(length(temp) == 0){
			temp<-sprintf("> %d",max(N.Sample.ALL))
			#print(temp)
			re[[i]]<-temp
		} else if( min(temp) ==1 ){
			re[[i]]<-min(N.Sample.ALL)
		}
		else {
			id1<-min(temp)
			re[[i]]<-(N.Sample.ALL[id1] - N.Sample.ALL[id1-1])/(Power.Est[id1,i] - Power.Est[id1-1,i]) * (Power - Power.Est[id1-1,i]) + N.Sample.ALL[id1-1]

		}
	}
	names(re)<-sprintf("alpha = %.2e",alpha)
	return(re)
}
