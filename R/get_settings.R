#' Check that all of the settings are in the list
#'
#' @param settings A list of the current settings where each object in the
#' list must be named. Those names that are not found in the stored list will
#' be added. The default value of \code{NULL} leads to a full list being returned.
#' @param verbose A logical value specifying if the message should be output
#' to the screen or not.
#'
#' @return A list of setting for running model diagnostics.
#'
#' @author Chantel Wetzel & Kelli Faye Johnson
#' @export
#'
#' @examples
#' get_settings(list("Njitter" = 10))
#' 
#' settings <- list()

get_settings <- function(settings = NULL, verbose = FALSE) {
  if (is.vector(settings)) settings <- as.list(settings)
  Settings = list(
  	base_name = "model", 
  	run = c("jitter", "profile_m", "profile_h", "profile_r0", "retro")
    )
  Settings_add <- list(
  	# Jitter Settings
    model = "ss",
    extras = "-nohess",
    Njitter = 100,
    Intern = TRUE,
    systemcmd = FALSE,
    printlikes = FALSE,
    verbose = FALSE,
    jitter_fraction = 0.05,
    init_values_src = NULL
    )

  Settings_all <- c(Settings, Settings_add)
  need <- !names(Settings_all) %in% names(settings)
  if (verbose) {
    message("Adding the following objects to settings:\n",
      paste(names(Settings_all[need]), collapse = "\n"), "\n",
      appendLF = TRUE)
  }
  Settings_all <- c(settings, Settings_all[need])

  return(Settings_all)
}