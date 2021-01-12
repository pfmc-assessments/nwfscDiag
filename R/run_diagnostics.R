#' Wrapper to run each of the 3 standard diagnostic items:
#' 1) Jitter
#' 2) Profiles across female m, steepness, and R0
#' 3) Retrospectives
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model_settings input of all settings created using the get_settings function
#' 
#' @author Chantel Wetzel
#' @return A vector of likelihoods for each jitter iteration.
#' @export

run_diagnostics <- function(mydir, model_settings ){

  if ("retro" %in% model_settings$run){
    retro_wrapper(mydir = mydir, model_settings = model_settings)
  
  } 

  if ("profile" %in% model_settings$run){
	   profile_wrapper(mydir = mydir, model_settings = model_settings)
  }

  if ("jitter" %in% model_settings$run){
    jitter_wrapper(mydir = mydir, model_settings = model_settings)  
  } 

}
