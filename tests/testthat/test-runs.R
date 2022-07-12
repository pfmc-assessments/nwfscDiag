### automated tests of nwfscDiag package
context("nwfscDiag functions that require executables to run")

# do runs in a temporary dir so that the state is not disrupted if tests
# exit early.
tmp_path <- file.path(tempdir(check = TRUE), "test-runs")
dir.create(tmp_path, showWarnings = FALSE)
example_path <- system.file("extdata", package = "nwfscDiag")
file.copy(example_path, tmp_path, recursive = TRUE)
# runs_path avoids repeated use of "extdata" that would have to be added
# if using tmp_path directly
runs_path <- file.path(tmp_path, "extdata")
# clean up
on.exit(unlink(tmp_path, recursive = TRUE))


test_that("Do profile using the simple model", {

	path <- file.path(runs_path, "simple")
    skip_if((!file.exists(file.path(path, "ss"))) &
      (!file.exists(file.path(path, "ss.exe"))),
    message = "SS executable missing"
    )

	get = get_settings_profile( parameters =  c("NatM_p_1_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
								low =  c(0.40, 0.25, -2),
								high = c(0.40, 1.0,  2),
								step_size = c(0.005, 0.05, 0.25),
								param_space = c('multiplier', 'real', 'relative'))

	model_settings = get_settings(settings = list(base_name = "simple",
											  run = c("profile"),
											  profile_details = get ))

	run_diagnostics(mydir = mydir, model_settings = model_settings)

	# Need to add checks here
})


test_that("Do jitters using the simple model", {

	path <- file.path(runs_path, "simple")
    skip_if((!file.exists(file.path(path, "ss"))) &
      (!file.exists(file.path(path, "ss.exe"))),
    message = "SS executable missing"
    )

	model_settings = get_settings(settings = list(base_name = "simple",
											  run = c("jitters")))

	run_diagnostics(mydir = mydir, model_settings = model_settings)

	# Need to add checks here
})


test_that("Do retrospectives using the simple model", {

	path <- file.path(runs_path, "simple")
    skip_if((!file.exists(file.path(path, "ss"))) &
      (!file.exists(file.path(path, "ss.exe"))),
    message = "SS executable missing"
    )

	model_settings <- get_settings(settings = list(base_name = "simple",
											  run = c("retro"),
											  retro_yrs = -1:-2))

	run_diagnostics(mydir = mydir, model_settings = model_settings)

	# Need to add checks here
	retro_subdirs <- file.path(
	    path, "retrospectives",
	    paste0("retro", c("0", "-1", "-2")))

	retro_ran <- lapply(
	  retro_subdirs,
	  function(d) file.exists(file.path(d, "Report.sso")))

	expect_true(all(unlist(retro_ran) == TRUE))
	
	# read model output from the retrospectives
	retroModels <- r4ss::SSgetoutput(
	  dirvec = file.path(
	    path_3.30.12, "retrospectives",
	    paste0("retro", 0:-2)))

	# summarize the model output
	retroSummary <- r4ss::SSsummarize(retroModels)
	
	# set the fector of ending years
	endyrvec <- retroSummary$endyrs + 0:-2
	r4ss::SSplotComparisons(retroSummary,
	  endyrvec = endyrvec,
	  legendlabels = paste("Data", 0:-2, "years")
	)
	# calculate Mohn's rho values
	# TODO: add better tests for mohns rho. Some values aren't calcualted b/c they
	# are missing in the summaries for this model run.
	mohns_rho <- r4ss::SSmohnsrho(retroSummary)
	expect_length(mohns_rho, 12)
})