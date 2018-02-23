SKAT_GET_kurtosis <- function(x) {  

	if(sd(x) == 0){
		return(-100)
	}
	m4 <- mean((x-mean(x))^4)
	kurt <- m4/(sd(x)^4)-3
	kurt
}
