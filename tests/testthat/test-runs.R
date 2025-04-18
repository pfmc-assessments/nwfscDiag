### automated tests of nwfscDiag package
#
# pak::pak() to make sure all dependencies are loaded <- did not work on my machine
# devtools::test()
#
# This is the directory where I want the tests to specifically run
tmp_path <- file.path("/test-runs-output")
dir.create(tmp_path, "test-runs-output", showWarnings = TRUE)

# Location where the simple model is saved in the package
runs_path <- system.file("extdata", package = "nwfscDiag")

# clean up (Comment out this if  you want to keep the files created by the tests)
on.exit(unlink(tmp_path, recursive = TRUE))

test_path <- file.path(runs_path, "simple_small")
skip_test <- TRUE
if (.Platform$OS.type == "windows") {
  if (file.exists(file.path(test_path, "ss3.exe"))) {
    skip_test <- FALSE
  }
}
if (.Platform$OS.type == "unix") {
  if (file.exists(file.path(test_path, "ss3"))) {
    skip_test <- FALSE
  }
}

test_that("Do profile using the simple model", {
  path <- file.path(runs_path)

  get <- get_settings_profile(
    parameters = c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
    low = c(0.20, 0.40, -0.5),
    high = c(0.20, 1.0, 0.5),
    step_size = c(0.01, 0.2, 0.25),
    param_space = c("multiplier", "real", "relative")
  )

  model_settings <- get_settings(
    settings = list(
      base_name = "simple_small",
      run = c("profile"),
      profile_details = get
    )
  )

  run_diagnostics(mydir = path, model_settings = model_settings)

  profile_para <- c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)")
  check <- 0
  for (a in 1:length(profile_para)) {
    check <- check +
      as.numeric(
        file.exists(
          file.path(
            path,
            paste0(
              model_settings$base_name, "_profile_",
              profile_para[a]
            ),
            paste0(profile_para[a], "_quant_table.csv")
          )
        )
      )
  }
  expect_true(check == length(profile_para))
})


test_that("Do jitters using the simple model", {
  path <- file.path(runs_path)

  model_settings <- get_settings(
    settings = list(
      base_name = "simple_small",
      run = c("jitter"),
      Njitter = 3
    )
  )

  run_diagnostics(mydir = path, model_settings = model_settings)

  jitter_folder <- file.path(
    path,
    paste0(model_settings$base_name, "_jitter_", model_settings$jitter_fraction)
  )

  expect_true(file.exists(file.path(jitter_folder, "jitter_results.csv")))
})


test_that("Do retrospectives using the simple model", {
  path <- file.path(runs_path)

  model_settings <- get_settings(
    settings = list(
      base_name = "simple_small",
      run = c("retro"),
      retro_yrs = -1:-2
    )
  )

  run_diagnostics(mydir = path, model_settings = model_settings)
  n <- length(model_settings$retro_yrs)
  check <- file.exists(
    file.path(path, paste0(model_settings$base_name, "_retro_", n, "_yr_peel"), "mohnsrho.csv")
  )
  expect_true(check)
})

test_that("Do mcmc diagnostics using the simple model", {
  # run_mcmc_diagnostics(
  #  dir_wd = test_path,
  #  model = "ss3",
  #  extension = "",
  #  iter = 1000,
  #  chains = 2,
  #  interactive = FALSE,
  #  verbose = FALSE
  # )

  # check <-  file.exists(
  #  file.path(path, "_mcmc", "fits", "mcmc.rds")
  # )
  # expect_true(check)
})
