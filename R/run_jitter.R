#' Run [r4ss::jitter] based on `model_settings`
#'
#' Code to run jitters for a model
#' Output will be saved in an Rdata object called "jitter_output"
#'
#'
#' @template mydir
#' @template model_settings
#'
#' @author Chantel Wetzel
#' @return
#' A list of quantities across jitters called `jitter_output`
#'
#'
#' @export

run_jitter <- function(mydir, model_settings) {
  if (!file.exists(file.path(mydir, model_settings[["base_name"]], "Report.sso"))) {
    base <- model_settings[["base_name"]]
    cli::cli_abort("There is no Report.sso file in the base model directory:
    {file.path(mydir, base}")
  }

  # Create a jitter folder with the same naming structure as the base model
  jitter_dir <- file.path(mydir, paste0(model_settings[["base_name"]], "_jitter_", model_settings[["jitter_fraction"]]))
  dir.create(jitter_dir, showWarnings = FALSE)
  all_files <- list.files(file.path(mydir, model_settings[["base_name"]]))
  utils::capture.output(file.copy(
    from = file.path(mydir, model_settings[["base_name"]], all_files),
    to = jitter_dir,
    overwrite = TRUE
  ), file = file.path(jitter_dir, "run_diag_warning.txt"))
  cli::cli_inform("Running jitters: temporarily changing working directory to: {jitter_dir}")

  r4ss::jitter(
    dir = jitter_dir,
    exe = model_settings[["exe"]],
    copy_exe = model_settings[["copy_exe"]],
    Njitter = model_settings[["Njitter"]],
    printlikes = model_settings[["printlikes"]],
    verbose = FALSE,
    jitter_fraction = model_settings[["jitter_fraction"]],
    init_values_src = model_settings[["jitter_init_values_src"]],
    extras = model_settings[["extras"]]
  )

  #### Read in results using other r4ss functions
  keys <- 1:model_settings[["Njitter"]]
  profilemodels <- r4ss::SSgetoutput(
    dirvec = jitter_dir,
    keyvec = keys,
    getcovar = FALSE,
    forecast = FALSE,
    verbose = FALSE,
    listlists = TRUE,
    underscore = FALSE,
    save.lists = FALSE
  )

  # summarize output
  profilesummary <- r4ss::SSsummarize(profilemodels)

  # Read in the base model
  base <- r4ss::SS_output(
    dir = file.path(mydir, model_settings$base_name),
    covar = FALSE,
    printstats = FALSE,
    verbose = FALSE
  )

  est <- base$likelihoods_used[1, 1]
  like <- as.numeric(profilesummary[["likelihoods"]][1, keys])
  ymax <- as.numeric(stats::quantile(unlist(profilesummary[["likelihoods"]][1, keys]), 0.80))
  ymin <- min(like - est) + 1

  jitter_output <- list()
  jitter_output[["plotdir"]] <- jitter_dir
  jitter_output[["est"]] <- est
  jitter_output[["keys"]] <- keys
  jitter_output[["like"]] <- like
  jitter_output[["model_settings"]] <- model_settings
  jitter_output[["profilesummary"]] <- profilesummary
  jitter_output[["profilemodels"]] <- profilemodels

  save(
    jitter_dir,
    est,
    keys,
    like,
    model_settings,
    profilesummary,
    profilemodels,
    file = file.path(jitter_dir, "jitter_output.Rdata")
  )
  return(jitter_output)
}
