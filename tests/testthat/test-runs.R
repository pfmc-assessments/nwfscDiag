### automated tests of nwfscDiag package

# do runs in a temporary dir so that the state is not disrupted if tests
# exit early.
tmp_path <- file.path("test", "test-runs")
dir.create(tmp_path, showWarnings = FALSE)
example_path <- system.file("extdata", package = "nwfscDiag")
file.copy(example_path, tmp_path, recursive = TRUE)
# runs_path avoids repeated use of "extdata" that would have to be added
# if using tmp_path directly
runs_path <- file.path(tmp_path, "extdata")
# clean up (Comment out this if  you want to keep the temp files)
on.exit(unlink(tmp_path, recursive = TRUE))


test_that("Do profile using the simple model", {

	test_path <- file.path(runs_path, "simple")
    skip_if((!file.exists(file.path(test_path, "ss"))) &
      (!file.exists(file.path(test_path, "ss.exe"))),
    message = "SS executable missing"
    )
    path <- file.path(runs_path)

	get <- get_settings_profile(parameters =  c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
								low =  c(0.20, 0.40, -1),
								high = c(0.20, 1.0,  1),
								step_size = c(0.01, 0.1, 0.25),
								param_space = c('multiplier', 'real', 'relative'))

	model_settings <- get_settings(settings = list(base_name = "simple",
											  run = c("profile"),
											  profile_details = get ))

	run_diagnostics(mydir = path, model_settings = model_settings)

	profile_para <- c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)")
	check <- 0
	for (a in 1:length(profile_para)){
		check <- check + 
			as.numeric(
				file.exists(
					file.path(path, 
						paste0(model_settings$base_name, "_profile_", 
						profile_para[a], "_prior_like_", get[a, "use_prior_like"]),
						paste0("profile_", profile_para[a], "_quant_table.csv")))
			)
	}
	expect_true(check == length(profile_para))
})


test_that("Do jitters using the simple model", {

	test_path <- file.path(runs_path, "simple")
    skip_if((!file.exists(file.path(test_path, "ss"))) &
      (!file.exists(file.path(test_path, "ss.exe"))),
    message = "SS executable missing"
    )
    path <- file.path(runs_path)

	model_settings <- get_settings(settings = list(base_name = "simple",
												   run = c("jitter"),
												   Njitter = 10))

	run_diagnostics(mydir = path, model_settings = model_settings)

	jitter_folder <- file.path(path, 
						paste0(model_settings$base_name, "_jitter_", 
							   model_settings$jitter_fraction))

	expect_true(file.exists(file.path(jitter_folder, "jitter_results.csv")))
})


test_that("Do retrospectives using the simple model", {

	test_path <- file.path(runs_path, "simple")
    skip_if((!file.exists(file.path(test_path, "ss"))) &
      (!file.exists(file.path(test_path, "ss.exe"))),
    message = "SS executable missing"
    )
    path <- file.path(runs_path)

	model_settings <- get_settings(settings = list(base_name = "simple",
											  run = c("retro"),
											  retro_yrs = -1:-2))

	run_diagnostics(mydir = path, model_settings = model_settings)

	check <- file.exists(
				file.path(path, paste0(model_settings$base_name, "_retro"),
						  "mohnsrho.csv"))
	expect_true(check)
})
