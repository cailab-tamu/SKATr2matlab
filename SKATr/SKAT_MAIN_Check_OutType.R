SKAT_MAIN_Check_OutType <- function(out_type) {
  if (out_type != "C" && out_type != "D") {
    stop(
      "Invalid out_type!. Please use either \"C\" for the continous outcome or \"D\" for the dichotomous outcome."
    )
  }

}
