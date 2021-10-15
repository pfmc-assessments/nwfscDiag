#' Wrapper to run each of the 3 standard diagnostic items:
#' 1) Jitter
#' 2) Profiles across female m, steepness, and R0
#' 3) Retrospectives
#' 
#'
#' @template mydir
#' @template model_settings
#' 
#' @author Chantel Wetzel
#' @return A vector of likelihoods for each jitter iteration.
#' @export

run_diagnostics <- function(mydir, model_settings ){

  if ("retro" %in% model_settings$run) {
    retro_wrapper(mydir = mydir, model_settings = model_settings)
  
  } 

  if ("profile" %in% model_settings$run) {
	   profile_wrapper(mydir = mydir, model_settings = model_settings)
  }

  if ("jitter" %in% model_settings$run) {
    jitter_wrapper(mydir = mydir, model_settings = model_settings)  
  } 

}
