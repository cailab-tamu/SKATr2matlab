SKAT_Check_Method <- function(method, r.corr) {
  if (method != "liu"  &&
      method != "davies" &&
      method != "liu.mod" &&
      method != "optimal" &&
      method != "optimal.moment" && method != "adjust") {
    stop("Invalid method!")
  }

  if ((method == "optimal" ||
       method == "optimal.moment") && length(r.corr) == 1) {
    r.corr = (0:10) / 10
    #r.corr = c(0, 0.1^2, 0.2^2, 0.3^2, 0.5^2, 0.5, 1)
  }
  if (method == "optimal") {
    method = "davies"
  } else if (method == "optimal.moment") {
    method = "liu.mod"
  }

  re <- list(method = method, r.corr = r.corr)
  return(re)

}
