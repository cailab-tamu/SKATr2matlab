SKAT_With_NullModel_ADJ = function(Z,
                                   obj.res.a,
                                   kernel = "linear.weighted",
                                   method = "adjust",
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
  obj.res <- obj.res.a$re1

  out.method <- SKAT_Check_Method(method, r.corr)
  method = out.method$method
  r.corr = out.method$r.corr

  SKAT_Check_RCorr(kernel, r.corr)
  # Use method field for the type of outcome
  method = obj.res.a$type

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

  res2 <- NULL
  if (obj.res.a$is_kurtosis_adj) {
    res2 <- obj.res.a$re2$res.out
  }

  if (length(r.corr) > 1 && dim(out.z$Z.test)[2] <= 1) {
    r.corr = 0
  }

  if (length(r.corr) == 1) {
    re = KMTest.logistic.Linear.VarMatching (
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
      ,
      obj.res$mu,
      res.moments = res2
    )

  } else {
    re = SKAT_Optimal_Logistic_VarMatching(
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
      r.corr,
      obj.res$mu
      ,
      res.moments = res2
    )

  }

  re$param$n.marker <- m
  re$param$n.marker.test <- dim(out.z$Z.test)[2]
  return(re)


}
