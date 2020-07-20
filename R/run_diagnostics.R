#' 
#'
#' 
#' 
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model Executable name of the \code{.exe} file in \code{mydir}
#'  without the extension, e.g., \code{"ss"}.
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @template verbose
#' @param 
#' @param 
#' @author Chantel Wetzel
#' @return A vector of likelihoods for each jitter iteration.
#' @export

library(HandyCode)
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/r4ss")
mydir = "C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscDiag/example"
model_settings = get_settings(settings = list(base_name = "21.0_mod",
											  Njitter = 2, jitter_fraction = 0.07))


run_diagnostics <- function(mydir, 
							model_settings, 
							){

  # Determine working directory on start and return upon exit
  startdir <- getwd()
  on.exit(setwd(startdir))

  # determine operating system in a relatively brute force way
  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0){
    OS <- "Linux"
  }
  if(length(grep("mingw",version$os)) > 0){
    OS <- "Windows"
  }

  setwd(paste0(mydir, "/", model_settings$base_name))
  if (length(dir(pattern = paste0("^",model_settings$model,"$","|",model_settings$model,"\\.exe"))) != 1) {
    stop(model_settings$model, " was not found in ", paste0(mydir, "/", model_settings$base_name), call. = FALSE)
  }

  if ("jitter" %in% model_settings$run){
  	jitter_wrapper(mydir = mydir, model_settings = model_settings)	
  }

  if ("profile_m" %in% model_settings$run){
	
  }

  if ("profile_h" %in% model_settings$run){
	
  }

  if ("profile_r0" %in% model_settings$run){
	
  }

  if ("retro" %in% model_settings$run){
	
  }  

}
