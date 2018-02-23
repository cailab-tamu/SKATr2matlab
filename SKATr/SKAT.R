SKAT = function(Z,
                obj,
                kernel = "linear.weighted",
                method = "davies",
                weights.beta = c(1, 25),
                weights = NULL,
                impute.method = "fixed",
                r.corr = 0,
                is_check_genotype = TRUE,
                is_dosage = FALSE,
                missing_cutoff = 0.15) {
  if (kernel != "linear" && kernel != "linear.weighted") {
    if (class(obj) == "SKAT_NULL_Model_ADJ") {
      msg <-
        sprintf(
          "The small sample adjustment only can be applied for linear and linear.weighted kernel in the current version of SKAT! No adjustment is applied"
        )
      warning(msg, call. = FALSE)
      obj <- obj$re1
    }

  }


  if (class(obj) == "SKAT_NULL_Model_ADJ") {
    re <-
      SKAT_With_NullModel_ADJ(
        Z,
        obj,
        kernel = kernel,
        method = method,
        weights.beta = weights.beta,
        weights = weights,
        impute.method = impute.method,
        r.corr = r.corr,
        is_check_genotype = is_check_genotype,
        is_dosage = is_dosage,
        missing_cutoff = missing_cutoff
      )

  } else if (class(obj) == "SKAT_NULL_Model") {
    re <-
      SKAT_With_NullModel(
        Z,
        obj,
        kernel = kernel,
        method = method,
        weights.beta = weights.beta,
        weights = weights,
        impute.method = impute.method,
        r.corr = r.corr,
        is_check_genotype = is_check_genotype,
        is_dosage = is_dosage,
        missing_cutoff = missing_cutoff
      )

  } else {
    #re<-SKAT_MAIN(Z,obj, ...)
    stop("The old interface is defunct! Please run SKAT_NULL_Model first!")
  }
  class(re) <- "SKAT_OUT"
  return(re)
}
