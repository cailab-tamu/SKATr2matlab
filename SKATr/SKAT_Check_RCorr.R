SKAT_Check_RCorr <- function(kernel, r.corr) {
  if (length(r.corr) == 1 && r.corr[1] == 0) {
    return(1)
  }
  if (kernel != "linear" && kernel != "linear.weighted") {
    stop("Error: non-zero r.corr only can be used with linear or linear.weighted kernels")
  }

  for (i in 1:length(r.corr)) {
    if (r.corr[i] < 0 || r.corr[i] > 1) {
      stop("Error: r.corr should be >= 0 and <= 1")
    }
  }
}
