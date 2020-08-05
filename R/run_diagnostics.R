#' 
#'
#' 
#' 
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model_settings input of all settings created using the get_settings function
#' 
#' @author Chantel Wetzel
#' @return A vector of likelihoods for each jitter iteration.
#' @export

run_diagnostics <- function(mydir, 
							              model_settings ){

  # Determine working directory on start and return upon exit
  startdir <- getwd()
  on.exit(setwd(startdir))

  # determine operating system in a relatively brute force way
  # OS <- "Mac" # don't know the version$os info for Mac
  # if(length(grep("linux",version$os)) > 0){
  #   OS <- "Linux"
  # }
  # if(length(grep("mingw",version$os)) > 0){
  #   OS <- "Windows"
  # }

  setwd(file.path(mydir, model_settings$base_name))
  if (length(dir(pattern = paste0("^",model_settings$model,"$","|",model_settings$model,"\\.exe"))) != 1) {
    stop(model_settings$model, " was not found in ", paste0(mydir, "/", model_settings$base_name), call. = FALSE)
  }


  if ("jitter" %in% model_settings$run){
  	jitter_wrapper(mydir = mydir, model_settings = model_settings)	
  }

  if ("profile" %in% model_settings$run){
	   profile_wrapper(mydir = mydir, model_settings = model_settings)
  }

  if ("profile_custom" %in% model_settings$run){
     para = model_settings$profile_para[!(model_settings$profile_para %in% c("female_m", "h", "r0"))]
     profile_wrapper(mydir = mydir, model_settings = model_settings, custom_para = para)
  }

  if ("retro" %in% model_settings$run){
    retro_wrapper(mydir = mydir, model_settings = model_settings)
	
  }  

}
