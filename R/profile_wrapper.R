#' Generate likelihood profiles
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model_settings input of all settings created using the get_settings function
#' @param para custom profile parameter
#' 
#' @author Chantel Wetzel.
#' @export

profile_wrapper <- function(mydir, model_settings, custom_para = NULL){

	OS <- "Mac" # don't know the version$os info for Mac
  	if(length(grep("linux", version$os)) > 0) OS <- "Linux"
  	if(length(grep("mingw", version$os)) > 0) OS <- "Windows"
	
  	# figure out name of executable based on 'model' input which may contain .exe
  	if(length(grep(".exe", tolower(file.path(mydir, model_settings$base_name)))) == 1){
  	  # if input 'model' includes .exe then assume it's Windows and just use the name
  	  exe <- model
  	}else{
  	  # if 'model' doesn't include .exe then append it (for Windows computers only)
  	  exe <- paste(model_settings$model, ifelse(OS == "Windows", ".exe", ""), sep="")
  	}
  	# check whether exe is in directory
  	if(OS == "Windows"){
  	  if(!tolower(exe) %in% tolower(file.path(mydir, model_settings$base_name))) 
  	  		stop("Executable ", exe, " not found in ", file.path(mydir, model_settings$base_name))
  	}else{
  	  if(!exe %in% file.path(mydir, model_settings$base_name)) 
  	  	stop("Executable ", exe, " not found in ", file.path(mydir, model_settings$base_name))
  	}

  	# Determine whether to do standard profile parameters or unique inputs
  	if (custom_para == NULL) { para = model_settings$profile_para[aa]
	} else {
		para = custom_para[aa]
	}

	for (aa in 1:length(para)){

	# Create a jitter folder with the same naming structure as the base model
	profile_dir <- file.path(mydir, paste0(model_settings$base_name, "_profile_", para[aa]))
  	dir.create(profile_dir, showWarnings = FALSE)

  	# Check for existing files and delete
  	if (model_settings$remove_files & length(list.files(profile_dir)) != 0) { 
  		remove <- list.files(profile_dir)
  		file.remove(remove) }

  	all_files <- list.files(file.path(mydir, model_settings$base_name)) 
  	capture.output(file.copy(from = file.path(mydir, model_settings$base_name, all_files), 
  			  to = profile_dir, overwrite = TRUE), file = "run_diag_warning.txt")
  	message(paste0( "Running profile for ", para, ".") )

  	# Read in the base model
  	base <- r4ss::SS_output(file.path(mydir, model_settings$base_name), printstats = FALSE, verbose = FALSE) 

  	# Copy the control file to run from the copy 
  	if (!file.exists(file.path(profile_dir, "control.ss_new"))){
  		command <- paste(model_settings$model, model_settings$extras)
      	if(OS != "windows") command <- paste("./", command, sep="")
      	cat("Running model in directory:", getwd(), "\n")
      	cat("Using the command: '", command, "'\n", sep="")
      	if(OS == "windows" & !model_settings$systemcmd){
      	  shell(cmd = command)
      	}else{
      	  system(command)
      }
  	}
  	file.copy(file.path(profile_dir, "control.ss_new"), file.path(profile_dir, model_settings$newctlfile))
  	# Change the control file name in the starter file
	starter <- SS_readstarter(file.path(profile_dir, 'starter.ss'))
	starter$ctlfile <- "control_modified.ss"
	starter$init_values_src <- model_settings$init_values_src
	# make sure the prior likelihood is calculated for non-estimated quantities
	starter$prior_like <- model_settings$prior_like
	r4ss::SS_writestarter(starter, dir = profile_dir, overwrite=TRUE) 

	# Determine the parameter range
	if( para[aa] == "female_m") {
	
	}
	#if( para[aa] == "male_m") {
	#
	#}
	if( para[aa] == "h") {
	
	}
	if( para[aa] == "r0"){
		r0_est <- round(base$parameters[base$parameters$Label == "SR_LN(R0)", "Value"], 2)
		vec <- sort(c(r0_est, seq(round(r0_est,0) - 2, round(r0_est,0) + 2, 0.25))) 
		para_name = 'SR_LN(R0)' }
    if( !(para %in% c("female_m", "h", "r0"))) {
	
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
	save(profilesummary, file.path(profile_dir, "profilesummary.R"))

	results = get_output(mydir = profile_dir, 
						 name = paste0("profile_", para),
						 profilemodels = profilemodels,
						 profilesummary = profilesummary)

	profile_plot(mydir = profile_dir, model_settings = model_settings, para = para_name, profilesummary = profilesummary)

	}
}