#' Retrospecitiv runs
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

	if(!file.exists(paste0(mydir, "/", model_settings$base_name, "/Report.sso"))) {
    	message("There is no Report.sso file in the base model directory", paste0(mydir, "/", model_settings$base_name))
    	stop()
  	}

	# Create a jitter folder with the same naming structure as the base model
	retro_dir <- paste0(mydir, "/", model_settings$base_name, "_retro")
  	dir.create(retro_dir, showWarnings = FALSE)
  	all_files = list.files(paste0(mydir, "/", model_settings$base_name)) 
  	capture.output(file.copy(from = paste0(mydir, "/", model_settings$base_name,"/", all_files), 
  			  to = retro_dir, 
  			  overwrite = TRUE), file = "run_diag_warning.txt")
  	message("Running retrospecitives")

  	r4ss::SS_doRetro(masterdir = retro_dir, 
  					 overwrite = model_settings$overwrite,
                     exefile = model_settings$model, 
                     extras = model_settings$extras,
  				     oldsubdir = model_settings$oldsubdir, 
  				     newsubdir = model_settings$newsubdir, 
  				     years = model_settings$retro_yrs)

  	capture.output(file.remove(from = paste0(retro_dir, "/", all_files)), file = "run_diag_warning.txt")

  	runs <- list()
  	for(aa in 1:(length(model_settings$retro_yrs) + 1)){
  		if (aa == 1) { runs[[aa]] = SS_output(paste0(mydir, "/", model_settings$base_name), verbose = FALSE, printstats = FALSE)
  		}else{
  			tmp = paste0(retro_dir, "/", model_settings$newsubdir, "/retro", model_settings$retro_yrs[aa-1])
  			runs[[aa]] = SS_output(tmp, verbose = FALSE, printstats = FALSE)
  		}
  	}

	retroSummary <- SSsummarize(runs)
	endyrvec <- c(retroSummary$endyrs[1], retroSummary$endyrs[1] + model_settings$retro_yrs)
	SSplotComparisons(retroSummary, 
		 			  endyrvec = endyrvec, 
		 			  legendlabels = c("Base Model", paste("Data", model_settings$retro_yrs, "Years")),
		 			  plotdir = retro_dir, 
    				  legendloc = "topright", 
    				  print = TRUE, 
    				  pdf = FALSE)

	# Placeholder for option to rename files to be retro specific
	#file.rename(paste0(retro_dir, "/plots/compare2_spawnbio_uncertainty.png"),paste0(retro_dir, "/plots/retro_ssb.png"))
	#file.rename(paste0(retro_dir, "/plots/compare4_Bratio_uncertainty.png"),  paste0(retro_dir, "/plots/retro_depl.png"))
	#file.rename(paste0(retro_dir, "/plots/compare9_recdevs.png"), 			  paste0(retro_dir, "/plots/retro_recdevs.png"))

}