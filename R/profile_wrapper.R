#' Generate likelihood profiles
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model_settings input of all settings created using the get_settings function
#' 
#' @author Chantel Wetzel.
#' @export

profile_wrapper <- function(mydir, model_settings, para){

	# Create a jitter folder with the same naming structure as the base model
	profile_dir <- file.path(mydir, paste0(model_settings$base_name, "_", para))
  	dir.create(profile_dir, showWarnings = FALSE)

  	# Check for existing files and delete
  	if (model_settings$remove_files) { 
  		remove <- list.files(profile_dir)
  		file.remove(remove) }

  	all_files <- list.files(file.path(mydir, model_settings$base_name)) 
  	capture.output(file.copy(from = file.path(mydir, model_settings$base_name, all_files), 
  			  to = profile_dir, 
  			  overwrite = TRUE), file = "run_diag_warning.txt")
  	message(cat( "Running ", profile ) )

  	# Read in the base model
  	base <- r4ss::SS_output(file.path(mydir, model_settings$base_name), printstats = FALSE, verbose = FALSE) 

  	# Copy the control file to run from the copy 
  	file.copy(file.path(profile_dir, "control.ss_new"), file.path(profile_dir, model_settings$newctlfile))
  	# Change the control file name in the starter file
	starter <- SS_readstarter(file.path(profile_dir, 'starter.ss'))
	starter$ctlfile <- "control_modified.ss"
	starter$init_values_src <- model_settings$init_values_src
	# make sure the prior likelihood is calculated for non-estimated quantities
	starter$prior_like <- model_settings$prior_like
	r4ss::SS_writestarter(starter, dir = profile_dir, overwrite=TRUE) 

	# Determine the parameter range
	if( para_range == 'default'){
		if( para == "profile_m") {
	
		}
		if( para == "profile_h") {
	
		}
		if( para == "profile_r0"){
			r0_est <- round(base$parameters[base$parameters$Label == "SR_LN(R0)", "Value"], 2)
			vec <- sort(c(r0_est, seq(round(r0_est,0) - 2, round(r0_est,0) + 2, 0.25))) }
			para_name = 'SR_LN(R0)'
	}

	Nprofile <- length(vec)

	profile <- r4ss::SS_profile(dir = profile_dir,
        						masterctlfile = "control.ss_new",
        						newctlfile = model_settings$newctlfile, 
        						string = para_name, 
        						profilevec = vec,
        						model = model_settings$model, 
        						extras = model_settings$extras, 
        						systemcmd = model_settings$systemcmd,
        						linenum = model_settings$linenum, 
    							string = model_settings$string, 
    							profilevec = model_settings$profilevec,
    							usepar = model_settings$usepar, 
    							globalpar = model_settings$globalpar, 
    							parfile = model_settings$parfile,
    							parlinenum = model_settings$parlinenum, 
    							parstring = model_settings$parstring,
    							dircopy = model_settings$dircopy, 
    							exe.delete = model_settings$exe.delete,
    							saveoutput = model_settings$saveoutput,
    							overwrite = model_settings$overwrite, 
    							whichruns = model_settings$whichruns, 
    							SSversion = model_settings$SSversion, 
    							prior_check = model_settings$prior_check,
    							read_like = model_settings$read_like)

	profilemodels <- SSgetoutput(dirvec = profile_dir, keyvec = 1:Nprofile)
	profilesummary <- SSsummarize(profilemodels)

}