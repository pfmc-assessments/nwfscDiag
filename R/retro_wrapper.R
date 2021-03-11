#' Retrospecitive runs
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model_settings input of all settings created using the get_settings function
#' 
#' @author Chantel Wetzel.
#' @export

retro_wrapper <- function(mydir,  model_settings){

	if(!file.exists(file.path(mydir, model_settings$base_name, "Report.sso"))) {
    	message("There is no Report.sso file in the base model directory", file.path(mydir, model_settings$base_name))
    	stop()
  	}

	# Create a jitter folder with the same naming structure as the base model
	retro_dir <- file.path(mydir, paste0(model_settings$base_name, "_retro"))
  dir.create(retro_dir, showWarnings = FALSE)
  all_files = list.files(file.path(mydir, model_settings$base_name)) 
  capture.output(file.copy(from = file.path(mydir, model_settings$base_name, all_files), 
  			  to = retro_dir, 
  			  overwrite = TRUE), file = "run_diag_warning.txt")
  message("Running retrospecitives")

  r4ss::SS_doRetro(masterdir = retro_dir, 
  				         overwrite = model_settings$overwrite,
                   exefile = model_settings$model, 
                   extras = model_settings$extras,
  			           oldsubdir = model_settings$oldsubdir, 
  			           newsubdir = model_settings$newsubdir, 
  			           years = model_settings$retro_yrs,
                   intern = model_settings$intern,
                   CallType = model_settings$CallType,
                   RemoveBlocks = model_settings$RemoveBlocks)

  capture.output(file.remove(from = file.path(retro_dir, all_files)), file = "run_diag_warning.txt")

  runs <- list()
  for(aa in 1:(length(model_settings$retro_yrs) + 1)){
  	if (aa == 1) { runs[[aa]] <- r4ss::SS_output(file.path(mydir, model_settings$base_name), verbose = FALSE, printstats = FALSE)
  	}else{
  		tmp = file.path(retro_dir, model_settings$newsubdir, paste0("retro", model_settings$retro_yrs[aa-1]))
  		runs[[aa]] <- r4ss::SS_output(tmp, verbose = FALSE, printstats = FALSE)
  	}
  }

	retroSummary <- r4ss::SSsummarize(runs)
	endyrvec <- c(retroSummary$endyrs[1], retroSummary$endyrs[1] + model_settings$retro_yrs)

  retro_output <- list()
  retro_output$plotdir <- retro_dir
  retro_output$endyrvec <- endyrvec
  retro_output$retroSummary <- retroSummary
  retro_output$model_settings <- model_settings
  save(retro_output, file = file.path(retro_dir, "retro_output.Rdata"))

	r4ss::SSplotComparisons(retroSummary, 
		 			                endyrvec = endyrvec, 
		 			                legendlabels = c("Base Model", paste("Data", model_settings$retro_yrs, "Years")),
		 			                plotdir = retro_dir, 
    				              legendloc = "topright", 
    				              print = TRUE, 
    				              pdf = FALSE)

	message("Finished retrospective runs.")
}