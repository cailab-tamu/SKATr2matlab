SKAT_With_NullModel = function(Z,
                               obj.res,
                               kernel = "linear.weighted",
                               method = "davies",
                               weights.beta = c(1, 25),
                               weights = NULL,
                               impute.method = "fixed",
                               r.corr = 0,
                               is_check_genotype = TRUE,
                               is_dosage = FALSE,
                               missing_cutoff = 0.15,
                               SetID = NULL) {
  n <- dim(Z)[1]
  m <- dim(Z)[2]

  out.method <- SKAT_Check_Method(method, r.corr)
  method = out.method$method
  r.corr = out.method$r.corr


  SKAT_Check_RCorr(kernel, r.corr)

  out.z <-
    SKAT_MAIN_Check_Z(
      Z,
      n,
      obj.res$id_include,
      SetID,
      weights,
      weights.beta,
      impute.method,
      is_check_genotype,
      is_dosage,
      missing_cutoff
    )
  if (out.z$return == 1) {
    out.z$param$n.marker <- m
    return(out.z)
  }

  if (length(r.corr) > 1 && dim(out.z$Z.test)[2] <= 1) {
    r.corr = 0
  }

  if (obj.res$out_type == "C") {
    if ((kernel == "linear" || kernel == "linear.weighted") && n > m) {
      re = SKAT.linear.Linear(
        obj.res$res,
        out.z$Z.test
        ,
        obj.res$X1,
        kernel,
        out.z$weights,
        obj.res$s2,
        method
        ,
        obj.res$res.out,
        obj.res$n.Resampling,
        r.corr = r.corr
      )
    } else {
      re = SKAT.linear.Other(
        obj.res$res,
        out.z$Z.test
        ,
        obj.res$X1,
        kernel,
        out.z$weights,
        obj.res$s2,
        method
        ,
        obj.res$res.out,
        obj.res$n.Resampling
      )
    }
  } else if (obj.res$out_type == "D") {
    if ((kernel == "linear" || kernel == "linear.weighted") && n > m) {
      re = SKAT.logistic.Linear(
        obj.res$res,
        out.z$Z.test
        ,
        obj.res$X1,
        kernel,
        out.z$weights,
        obj.res$pi_1,
        method
        ,
        obj.res$res.out,
        obj.res$n.Resampling,
        r.corr = r.corr
      )
    } else {
      re = SKAT.logistic.Other(
        obj.res$res,
        out.z$Z.test
        ,
        obj.res$X1,
        kernel,
        out.z$weights,
        obj.res$pi_1,
        method
        ,
        obj.res$res.out,
        obj.res$n.Resampling
      )
    }
  }

  re$param$n.marker <- m
  re$param$n.marker.test <- dim(out.z$Z.test)[2]
  return(re)

}
