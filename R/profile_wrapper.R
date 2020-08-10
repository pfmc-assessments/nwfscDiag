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

profile_wrapper <- function(mydir, model_settings){

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
  	  if(!exe %in% list.files(file.path(mydir, model_settings$base_name))) 
  	  		stop("Executable ", exe, " not found in ", file.path(mydir, model_settings$base_name))
  	}else{
  	  if(!exe %in% list.files(file.path(mydir, model_settings$base_name))) 
  	  	stop("Executable ", exe, " not found in ", file.path(mydir, model_settings$base_name))
  	}

  	N <- length(model_settings$profile_para) + length(model_settings$profile_custom)
  	list <- c(model_settings$profile_para, model_settings$profile_custom)

	for (aa in 1:N){
  	
  	para = list[aa]

  	# Create a profile folder with the same naming structure as the base model
	profile_dir <- file.path(mydir, paste0(model_settings$base_name, "_profile_", para))
  	dir.create(profile_dir, showWarnings = FALSE)

  	# Check for existing files and delete
  	if (model_settings$remove_files & length(list.files(profile_dir)) != 0) { 
  		remove <- list.files(profile_dir) 
  		file.remove(file.path(profile_dir, remove)) 
  	}

  	all_files <- list.files(file.path(mydir, model_settings$base_name))
  	capture.output(file.copy(from = file.path(mydir, model_settings$base_name, all_files), 
  			  to = profile_dir, overwrite = TRUE), file = "run_diag_warning.txt")
  	message(paste0( "Running profile for ", para, ".") )

  	# Determine whether to do standard profile parameters or unique inputs
  	if (para %in% model_settings$profile_para) {   		
  		if( para == "female_m") { para_name <- 'NatM_p_1_Fem_GP_1'  }
  		if( para == "male_m")   { para_name <- 'NatM_p_1_Mal_GP_1'  }
  		if( para == "h")        { para_name <- 'SR_BH_steep'        }
  		if( para == "r0")       { para_name <- 'SR_LN(R0)'          }
	} else {
		para_name <- para
	    # Use the SS_parlines funtion to ensure that the input parameter can be found
		check_para <- r4ss::SS_parlines(ctlfile="control.ss_new", 
							      dir = profile_dir, 
							      verbose = FALSE, 
							      active = FALSE)$Label == para_name
		if( sum(check_para) == 0) {
			stop(paste0( "The input profile_custom does not match a parameter in the control.ss_new file."))
		}
	}

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
	starter <- r4ss::SS_readstarter(file.path(profile_dir, 'starter.ss'))
	starter$ctlfile <- "control_modified.ss"
	starter$init_values_src <- model_settings$profile_init_values_src
	# make sure the prior likelihood is calculated for non-estimated quantities
	starter$prior_like <- model_settings$prior_like
	r4ss::SS_writestarter(starter, dir = profile_dir, overwrite=TRUE) 

  	# Read in the base model
  	base <- r4ss::SS_output(file.path(mydir, model_settings$base_name), covar = FALSE,
  							printstats = FALSE, verbose = FALSE)
	est  <- base$parameters[base$parameters$Label == para_name, "Value"]

	# Determine the parameter range
	if( para %in% c("female_m", "male_m")) {
		if (model_settings$para_range_m == 'default'){
			if (est <= 0.10) { range <- c( est - est * 0.50, est + est * 0.50 ) }
			if (est >  0.10) { range <- c( est - est * 0.25, est + est * 0.25 ) }
			step_size <- ifelse(range[2] - range[1] > 0.10, 0.01, 0.005 ) 
		} else {
			range <- c(model_settings$para_range_m[1], model_settings$para_range_m[2])
			step_size <- model_settings$para_range_m[3]
		}
	}

	if( para == "h") {
		range <- c(model_settings$para_range_h[1], model_settings$para_range_h[2])
		step_size <- model_settings$para_range_h[3]
	}	

	if( para == "r0"){
		range <- c(  floor(est - model_settings$para_range_r0[1]), 
				   ceiling(est + model_settings$para_range_r0[2]))
		step_size <- model_settings$para_range_r0[3]
	}

	if( !(list[aa] %in% c("h", "female_m", "male_m", "r0"))){
		range <- c(model_settings$para_custom_range[1], model_settings$para_custom_range[2])
		step_size <- model_settings$para_custom_range [3]
	}

	# Create parameter vect from base down and the base up
	low  <- rev(seq(range[1], plyr::round_any(est, step_size, f = floor), step_size))
	high <- c(est, seq(plyr::round_any(est, step_size, f = ceiling), range[2], step_size))
	vec  <- c(low, high)
	num <- sort(vec, index.return = TRUE)$ix

	profile <- r4ss::SS_profile(dir = profile_dir,
        						masterctlfile = "control.ss_new",
        						newctlfile = model_settings$newctlfile, 
        						string = para_name, 
        						profilevec = vec,
        						model = model_settings$model, 
        						extras = model_settings$extras, 
        						systemcmd = model_settings$systemcmd,
        						linenum = model_settings$linenum, 
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


	profilemodels <- SSgetoutput(dirvec = profile_dir, keyvec = num)
	profilesummary <- SSsummarize(profilemodels)
	save(profilesummary, file = file.path(profile_dir, "profilesummary.Rdat"))

	results = get_summary(mydir = profile_dir, 
						 name = paste0("profile_", para),
						 para = para,
						 vec = vec[num],
						 profilemodels = profilemodels,
						 profilesummary = profilesummary)

	profile_plot(mydir = profile_dir, 
				 model_settings = model_settings, 
				 para_name = para_name, 
				 rep = base,
				 vec = vec[num],
				 profilesummary = profilesummary)

	message("Finished profile of ", para, ".")

	} #end parameter loop

}