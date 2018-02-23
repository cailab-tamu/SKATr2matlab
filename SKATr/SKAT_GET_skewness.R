SKAT_GET_skewness <-  function(x) {
	m3 <- mean((x-mean(x))^3)
	skew <- m3/(sd(x)^3)
	return(skew)
}
