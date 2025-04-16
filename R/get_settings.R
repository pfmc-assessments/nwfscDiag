#' Check that all of the settings are in the list
#'
#' @template mydir
#' @param settings A list of the current settings where each object in the list
#'   must be named. Those names that are not found in the stored list will be
#'   added. The default value of \code{NULL} leads to a full list being
#'   returned.
#'
#' @return
#' A list of settings for running model diagnostics.
#'
#' @author Chantel Wetzel & Kelli Faye Johnson
#' @export
#'
#' @examples
#' \dontrun{
#' get_settings(
#'   mydir = "directory",
#'   settings = list("Njitter" = 10)
#' )
#' }
#'
get_settings <- function(mydir = NULL, settings = NULL) {
  if (is.vector(settings)) settings <- as.list(settings)

  Settings_all <- list(
    base_name = "model",
    para_offset = FALSE,
    run = c("jitter", "profile", "retro"),
    profile_details = NULL,
    version = "3.30",
    exe = "ss3",
    copy_exe = TRUE,
    verbose = TRUE,

    # Jitter Settings
    extras = "-nohess",
    Njitter = 100,
    show_in_console = TRUE,
    printlikes = FALSE,
    jitter_fraction = 0.05,
    jitter_init_values_src = NULL,

    # Retrospective Settings
    oldsubdir = "",
    newsubdir = "retro",
    retro_yrs = -1:-5,
    overwrite = TRUE,
    show_in_console = FALSE,

    # Plot target lines
    btarg = NULL,
    minbthresh = NULL,

    # Profile Settings
    remove_files = TRUE,
    oldctlfile = "control.ss_new",
    newctlfile = "control_modified.ss",
    linenum = NULL,
    string = NULL,
    profilevec = NULL,
    usepar = FALSE,
    globalpar = FALSE,
    parlinenum = NULL,
    parstring = NULL,
    saveoutput = TRUE,
    overwrite = TRUE,
    whichruns = NULL,
    prior_check = FALSE,
    init_values_src = 0,
    subplots = c(1, 3)
  )

  Settings_all[["profile_details"]] <- get_settings_profile()

  need <- !names(Settings_all) %in% names(settings)
  Settings_all <- c(settings, Settings_all[need])

  # Check some items
  if (!is.null(Settings_all[["profile_details"]])) {
    if (length(Settings_all[["profile_details"]][is.na(Settings_all[["profile_details"]])]) > 0) {
      cli::cli_abort(
        "Missing entry in the get_settings_profile data frame."
      )
    }
    if (!is.numeric(Settings_all[["profile_details"]][["low"]]) &
      !is.numeric(Settings_all[["profile_details"]][["high"]]) &
      !is.numeric(Settings_all[["profile_details"]][["step_size"]])) {
      cli::cli_abort("There is a non-numeric value in the low, high, or step size field of the get_settings_profile data frame.")
    }
    if (sum(!Settings_all[["profile_details"]][["param_space"]] %in% c("real", "relative", "multiplier")) > 0) {
      cli::cli_abort("The param_space column should be either real or relative in the get_settings_profile data frame.")
    }
  }

  if ("profile" %in% Settings_all[["run"]]) {
    if (is.null(mydir) & Settings_all[["verbose"]]) {
      cli::cli_inform(
        "The directory (mydir) is not provided. Profile parameter names
        not checked and the profile range not be reported. To check profile
        information specify mydir and add verbose = TRUE to the settings list."
      )
    }
    if (!is.null(mydir)) {
      check_profile_range(
        mydir = mydir,
        model_settings = Settings_all
      )
    }
  }

  return(Settings_all)
}
