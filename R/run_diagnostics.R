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

  # Check for Report file
  model_dir <- file.path(mydir, paste0(model_settings$base_name))
  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux", version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw", version$os)) > 0) OS <- "Windows"

  if (!file.exists(file.path(model_dir, "Report.sso"))) {
    orig_dir <- getwd()
    setwd(model_dir)
    command <- paste(model_settings$model, model_settings$extras)
    if(OS != "windows") {
      command <- paste( "./", command, sep="")
    }
    cat("Running model in directory:", getwd(), "\n")
    cat("Using the command: '", command, "'\n", sep="")
    if(OS == "windows" & !model_settings$systemcmd){
      shell(cmd = command)
    } else {
      system(command)
    }
    setwd(orig_dir)
  }

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
