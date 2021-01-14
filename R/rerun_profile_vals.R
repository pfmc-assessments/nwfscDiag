#' Generate likelihood profiles
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#' 
#'
#' @param mydir Directory where model files are located.
#' @param para_name SS parameter name
#' @param run_num A single or vector of run numbers that you would like to rerun for convergence
#' @param data_file_nm SS data file name 
#'
#' @author Chantel Wetzel.
#' @export
rerun_profile_vals <- function(mydir,
							   para_name,
							   run_num,
							   data_file_nm)
{

	if(missing(mydir)){
		stop("Stop: Need to specify mydir.")
	}

	if(missing(run_num)){
		stop("Stop: Need to specify run_num.")
	}

	if(missing(para_name)){
		stop("Stop: Need to specify parameter name via parameter function input.")
	}
	para = para_name

	profile_dir = paste0(mydir, "_profile_", para_name)
	temp_dir = file.path(profile_dir, "temp")
	dir.create(temp_dir, showWarnings = FALSE)

	file.copy(file.path(profile_dir, 'ss.exe'), temp_dir)
	file.copy(file.path(profile_dir, "control_modified.ss"), temp_dir)
	file.copy(file.path(profile_dir, "control.ss_new"), temp_dir)
	file.copy(file.path(profile_dir, data_file_nm), temp_dir)
	file.copy(file.path(profile_dir, "starter.ss_new"), temp_dir)
	file.copy(file.path(profile_dir, "forecast.ss_new"), temp_dir)
	file.rename(file.path(temp_dir, "starter.ss_new"), file.path(temp_dir, "starter.ss"))
	file.rename(file.path(temp_dir, "forecast.ss_new"), file.path(temp_dir, "forecast.ss"))

	# Use the SS_parlines funtion to ensure that the input parameter can be found
	check_para <- r4ss::SS_parlines(dir = temp_dir, 
						      		verbose = FALSE, 
						      		active = FALSE)$Label == para
	if( sum(check_para) == 0) {
		stop(paste0( "The input profile_custom does not match a parameter in the control.ss_new file."))
	}

	load(file.path(profile_dir, paste0(para_name, "_profile_output.Rdat")))
	model_settings <- profile_output$model_settings
	vec <- profile_output$vec_unordered 
  	num <- profile_output$num
  	rep <- profile_output$rep
  	like_check = profile_output$profilesummary$likelihoods[1,]

	# Change the control file name in the starter file
	starter <- r4ss::SS_readstarter(file.path(temp_dir, 'starter.ss'))
	starter$jitter_fraction <- 0.01
	starter$init_values_src <- model_settings$profile_init_values_src
	# make sure the prior likelihood is calculated for non-estimated quantities
	starter$prior_like <- model_settings$prior_like
	r4ss::SS_writestarter(starter, dir = temp_dir, overwrite=TRUE) 
	
	
	for(i in run_num){
		setwd(temp_dir)
		r4ss::SS_changepars(
				ctlfile = "control_modified.ss", 
                newctlfile = "control_modified.ss", 
                strings = para, 
                newvals = vec[run_num], 
                estimate = FALSE)
		system("ss -nohess")

		mod = r4ss::SS_output(temp_dir, covar = FALSE,printstats = FALSE, verbose = FALSE)
		like = mod$likelihoods_used[1,1]

		# See if likelihood is lower than the original - and rerun if not
		add = 0.01
		if(like > like_check[run_num]){
			for(ii in 1:5){
				starter <- r4ss::SS_readstarter(file.path(temp_dir, 'starter.ss'))
				starter$jitter_fraction <- add + starter$jitter_fraction
				r4ss::SS_writestarter(starter, dir = temp_dir, overwrite=TRUE) 
				system("ss -nohess")
				mod = r4ss::SS_output(temp_dir, covar = FALSE,printstats = FALSE, verbose = FALSE)
				like = mod$likelihoods_used[1,1]
				if( like < like_check[run_num] ){ break() }
			}
		}

		files = c("CompReport", "covar", "Report", "warning")
		for(j in 1:length(files)){
			file.rename(paste0(files[j], ".sso"),
						paste0(files[j], run_num, ".sso"))
			file.copy(paste0(files[j], run_num, ".sso"),
					  profile_dir, overwrite = TRUE)
		}
		file.rename("ss.par", paste0("ss.par_", run_num, ".sso"))
		file.copy(paste0("ss.par_", run_num, ".sso"), profile_dir, overwrite = TRUE)

	}

	profilemodels  <- r4ss::SSgetoutput(dirvec = profile_dir, keyvec = num)
	profilesummary <- r4ss::SSsummarize(profilemodels)

	protect = file.path(profile_dir, "protect")
  	dir.create(protect, showWarnings = FALSE)
  	save_files = c(paste0(para, "_trajectories_compare1_spawnbio.png"),
  				   paste0(para, "_trajectories_compare3_Bratio.png"),
  				   paste0("parameter_panel_", para, ".png"),
  				   paste0("piner_panel_", para, ".png"),
  				   paste0("profile_", para, "_parsonbounds.csv"),
  				   paste0("profile_", para, "_quant_table.csv"),
  				   paste0("profile_", para, "_results.csv"),
  				   paste0(para, "_profile_output.Rdat"))
  	for(i in 1:length(save_files)){
  		file.copy(file.path(profile_dir, save_files[i]), 
  				  file.path(profile_dir, "protect"))
  	}

  	profile_output <- list()
  	profile_output$mydir <- profile_dir
  	profile_output$para <- para
  	profile_output$name <- paste0("profile_", para)
  	profile_output$vec <- vec[num]
  	profile_output$model_settings <- model_settings
  	profile_output$profilemodels <- profilemodels
  	profile_output$profilesummary <- profilesummary
  	profile_output$rep <- rep
  	profile_output$vec_unordered <- vec
  	profile_output$num <- num

  	save(profile_output, file = file.path(profile_dir, paste0(para, "_profile_output.Rdat")))

	get_summary(mydir = profile_dir, 
				name = paste0("profile_", para),
				para = para,
				vec = vec[num],
				profilemodels = profilemodels,
				profilesummary = profilesummary)

	profile_plot(mydir = profile_dir, 
				model_settings = model_settings, 
				para = para, 
				rep = rep,
				vec = vec[num],
				profilesummary = profilesummary)

}