#' Wrapper to run each of the 3 standard diagnostic items:
#' 1) Jitter
#' 2) Profiles across requested SS3 parameters
#' 3) Retrospectives
#'
#'
#' @template mydir
#' @template model_settings
#'
#' @author Chantel Wetzel
#' @return A vector of likelihoods for each jitter iteration.
#' @export

run_diagnostics <- function(mydir, model_settings) {
  exe <- r4ss::check_exe(exe = model_settings$exe, dir = file.path(mydir, model_settings[["base_name"]]))[["exe"]]
  model_settings[["exe"]] <- exe

  # Check for Report file
  model_dir <- file.path(mydir, paste0(model_settings[["base_name"]]))

  if (!file.exists(file.path(model_dir, "Report.sso"))) {
    orig_dir <- getwd()
    setwd(model_dir)
    cli::cli_info("Running model in directory: {getwd()}")
    r4ss::run(
      dir = model_dir,
      exe = model_settings[["exe"]],
      extras = model_settings[["extras"]],
      skipfinished = FALSE,
      verbose = model_settings[["verbose"]]
    )
    setwd(orig_dir)
  }

  if ("retro" %in% model_settings[["run"]]) {
    retro_wrapper(mydir = mydir, model_settings = model_settings)
  }

  if ("profile" %in% model_settings[["run"]]) {
    profile_wrapper(mydir = mydir, model_settings = model_settings)
  }

  if ("jitter" %in% model_settings[["run"]]) {
    jitter_wrapper(mydir = mydir, model_settings = model_settings)
  }
}
