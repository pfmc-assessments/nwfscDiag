#' Get Default Settings For Profiles
#' 
#' Create a matrix of default values for profiling over
#' the typical parameters given results will be presented to the
#' Pacific Fisheries Management Council.
#' 
#' @return A matrix of low and high values for the default parameters
#' that should be profiled. The goal is to provide users with a template
#' to add additional rows for parameters that they want to profile beyond
#' the default ones.
#'
#' @examples
#' get_settings_profile()
#'
get_settings_profile <- function() {
  out <- matrix(NA, ncol = 2, nrow = 3)
  rownames(out) <- c("NatM_p_1_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)")
  colnames(out) <- c("low", "high")
  out[, "low"] <- c(0.1, 0.2, 10)
  out[, "high"] <- c(2, 1.0, 20)
  return(out)
}
