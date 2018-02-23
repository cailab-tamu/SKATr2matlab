SKAT.linear = function(Z,y, X1, kernel = "linear", weights = NULL, method="liu", res.out=NULL, n.Resampling = 0, r.corr=r.corr){

  n = length(y)
  m = ncol(Z)

  mod = lm(y~X1 -1)
  s2 = summary(mod)$sigma**2
  res = mod$resid

  # If m >> p and ( linear or linear.weight) kernel than call
  # Linear function
  if( (kernel =="linear" || kernel == "linear.weighted") && n > m){
    re = SKAT.linear.Linear(res,Z,X1, kernel, weights,s2,method,res.out,n.Resampling,r.corr)
  } else {
    re = SKAT.linear.Other(res,Z,X1, kernel, weights,s2,method,res.out,n.Resampling)
  }

  return(re)
}
