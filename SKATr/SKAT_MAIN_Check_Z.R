SKAT_MAIN_Check_Z <-
  function(Z,
           n,
           id_include,
           SetID,
           weights,
           weights.beta,
           impute.method,
           is_check_genotype,
           is_dosage,
           missing_cutoff) {
    #############################################
    # Check parameters

    if (class(Z) != "matrix")
      stop("Z is not a matrix")
    if (nrow(Z) != n)
      stop("Dimensions of y and Z do not match")
    if (is_dosage == TRUE) {
      impute.method = "fixed"
    }
    #####################################################
    # Check Z

    if (!is_check_genotype && !is_dosage) {
      Z.test <- Z[id_include, ]
      if (!is.matrix(Z.test)) {
        Z.test <- as.matrix(Z.test)
      }
      return(list(
        Z.test = Z.test,
        weights = weights,
        return = 0
      ))
    }

    ##############################################
    # Check Missing

    IDX_MISS <- union(which(is.na(Z)), which(Z == 9))
    if (length(IDX_MISS) > 0) {
      Z[IDX_MISS] <- NA
    }

    ###################################################
    # Check missing rates and exclude any SNPs with missing rate > missing_cutoff
    # Also exclude non-polymorphic SNPs
    m = ncol(Z)
    ID_INCLUDE_SNP <- NULL
    for (i in 1:m) {
      missing.ratio <- length(which(is.na(Z[, i]))) / n
      sd1 <- sd(Z[, i], na.rm = TRUE)
      if (missing.ratio < missing_cutoff && sd1 > 0) {
        ID_INCLUDE_SNP <- c(ID_INCLUDE_SNP, i)
      }
    }

    if (length(ID_INCLUDE_SNP) == 0) {
      if (is.null(SetID)) {
        msg <-
          sprintf("ALL SNPs have either high missing rates or no-variation. P-value=1")
      } else {
        msg <-
          sprintf("In %s, ALL SNPs have either high missing rates or no-variation. P-value=1",
                  SetID)
      }
      warning(msg, call. = FALSE)

      re <-
        list(
          p.value = 1,
          p.value.resampling = NA,
          Test.Type = NA,
          Q = NA,
          param = list(n.marker = 0, n.marker.test = 0),
          return = 1
        )

    } else if (m - length(ID_INCLUDE_SNP) > 0) {
      if (is.null(SetID)) {
        msg <-
          sprintf(
            "%d SNPs with either high missing rates or no-variation are excluded!",
            m - length(ID_INCLUDE_SNP)
          )
      } else {
        msg <-
          sprintf(
            "In %s, %d SNPs with either high missing rates or no-variation are excluded!",
            SetID,
            m - length(ID_INCLUDE_SNP)
          )
      }

      warning(msg, call. = FALSE)
      Z <- as.matrix(Z[, ID_INCLUDE_SNP])
    }


    ##################################################################
    # doing imputation

    MAF <- colMeans(Z, na.rm = TRUE) / 2
    MAF1 <- colMeans(as.matrix(Z[id_include, ]), na.rm = TRUE) / 2
    IDX.Err <- which(MAF > 0.5)
    if (length(IDX.Err) > 0) {
      #msg<-sprintf("Genotypes of some variants are not the number of minor allele! It is fixed!")
      msg <-
        sprintf("Genotypes of some variants are not the number of minor alleles!")
      warning(msg, call. = FALSE)

      # Fixed by SLEE
      #Z[,IDX.Err]<-2 - Z[,IDX.Err]
      #MAF[IDX.Err]<-1- MAF[IDX.Err]
    }

    ###########################################
    # Check non-polymorphic

    if (length(which(MAF1 > 0)) == 0) {
      if (is.null(SetID)) {
        msg <- sprintf("No polymorphic SNP. P-value = 1")
      } else {
        msg <- sprintf("In %s, No polymorphic SNP. P-value = 1", SetID)
      }
      warning(msg, call. = FALSE)
      re <-
        list(
          p.value = 1,
          p.value.resampling = NA,
          Test.Type = NA,
          Q = NA,
          param = list(n.marker = 0, n.marker.test = 0),
          return = 1
        )
      return(re)
    }

    ##########################################
    # Missing Imputation
    if (length(IDX_MISS) > 0) {
      if (is.null(SetID)) {
        msg <-
          sprintf("The missing genotype rate is %f. Imputation is applied.",
                  (length(IDX_MISS)) / length(Z))
      } else {
        msg <-
          sprintf(
            "In %s, the missing genotype rate is %f. Imputation is applied.",
            SetID,
            (length(IDX_MISS)) / length(Z)
          )
      }

      warning(msg, call. = FALSE)
      Z <- Impute(Z, impute.method)
    }

    ##########################################
    # Get Weights

    if (is.null(weights)) {
      weights <- Beta.Weights(MAF, weights.beta)
    }

    ###########################################
    # Check missing of y and X

    if (n - length(id_include)  > 0) {
      id_Z <- which(MAF1 > 0)

      if (length(id_Z) == 0) {
        if (is.null(SetID)) {
          msg <- sprintf("No polymorphic SNP. P-value = 1")
        } else {
          msg <- sprintf("In %s, No polymorphic SNP. P-value = 1", SetID)
        }
        warning(msg, call. = FALSE)
        re <-
          list(
            p.value = 1,
            p.value.resampling = NA,
            Test.Type = NA,
            Q = NA,
            param = list(n.marker = 0, n.marker.test = 0),
            return = 1
          )

      } else if (length(id_Z) == 1) {
        Z <- cbind(Z[, id_Z])
      } else {
        Z <- Z[, id_Z]
      }

      if (!is.null(weights)) {
        weights <- weights[id_Z]
      }

    }

    if (dim(Z)[2] == 1) {
      if (is.null(SetID)) {
        msg <- sprintf("Only one SNP in the SNP set!")
      } else {
        msg <- sprintf("In %s, Only one SNP in the SNP set!"
                       , SetID)
      }
      warning(msg, call. = FALSE)

      Z.test <- as.matrix(Z[id_include, ])

    } else {
      Z.test <- Z[id_include, ]

    }

    return(list(
      Z.test = Z.test,
      weights = weights,
      return = 0
    ))

  }
