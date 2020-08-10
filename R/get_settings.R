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

  Settings_all = list(
    base_name = "model", 
    para_offset = FALSE,
    run = c("jitter", "profile", "retro"),
    profile_para = c("female_m", "h", "r0"),
    profile_custom = NULL,

  	# Jitter Settings
    model = "ss",
    extras = "-nohess",
    Njitter = 100,
    Intern = TRUE,
    systemcmd = FALSE,
    printlikes = FALSE,
    verbose = FALSE,
    jitter_fraction = 0.05,
    jitter_init_values_src = NULL,
    
    # Retrospective Settings
    oldsubdir = "",
    newsubdir = 'retro',
    retro_yrs = -1:-5,
    overwrite = TRUE,
    intern = FALSE,
    CallType = "system",
    RemoveBlocks = FALSE,
    
    # Profile Settings
    para_range_m = 'default',
    para_range_h = c(0.25, 1.0, 0.05), # Absolute parameter scale
    para_range_r0 = c(2, 2, 0.25), # Relative to R0 where this is R0 - 2 and R0 + 2
    para_custom_range = NULL, # needs to be seq( low, high, step size)
    remove_files = TRUE,
    newctlfile = "control_modified.ss", 
    profile_init_values_src = 0, 
    prior_like = 0, # turn off the prior contribution to likelihood
    linenum = NULL, 
    string = NULL, 
    profilevec = NULL,
    usepar = FALSE, 
    globalpar = FALSE, 
    parfile = "ss.par",
    parlinenum = NULL, 
    parstring = NULL,
    dircopy = TRUE, 
    exe.delete = FALSE,
    saveoutput = TRUE,
    whichruns = NULL, 
    SSversion = "3.30", 
    prior_check = FALSE,
    read_like = TRUE
    )

  need <- !names(Settings_all) %in% names(settings)
  if (verbose) {
    message("Adding the following objects to settings:\n",
      paste(names(Settings_all[need]), collapse = "\n"), "\n",
      appendLF = TRUE)
  }
  Settings_all <- c(settings, Settings_all[need])

  # Check some items
  if (!is.null( Settings_all$profile_custom)) {
    if( is.null(Settings_all$para_custom_range )){
      stop("If doing a custom parameter you must include the para_custom_range.  The expected input is c(low, high, step size).")
    }
    message("Make sure the profile_custom setting matches the string for the control.ss_new for your paticular parameter.")
  }


  return(Settings_all)
}