SKAT_1 = function(Z, obj, ...) {
  if (class(obj) == "SKAT_NULL_Model_ADJ") {
    re <- SKAT_With_NullModel_ADJ(Z, obj, ...)
  } else if (class(obj) == "SKAT_NULL_Model") {
    re <- SKAT_With_NullModel(Z, obj, ...)
  } else {
    #re<-SKAT_MAIN(Z,obj, ...)
    stop("The old interface is defunct! Please run SKAT_NULL_Model first!")
  }
  class(re) <- "SKAT_OUT"
  return(re)
}
