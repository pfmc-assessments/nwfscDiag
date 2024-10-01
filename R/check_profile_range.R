#' Check the profile range specified
#'
#'
#' @template mydir
#' @template model_settings
#'
#' @author Chantel Wetzel
#' @return
#' Nothing is explicitly returned from `check_profile_range()`
#' @export
#'
#'
check_profile_range <- function(mydir, model_settings){
  # Read in the base model
  rep <- r4ss::SS_output(
    file.path(mydir, model_settings$base_name),
    covar = FALSE,
    printstats = FALSE,
    verbose = FALSE
  )

  N <- nrow(model_settings$profile_details)
  for (aa in 1:N){
    profile_details <- model_settings[["profile_details"]][aa, ]
    para <- profile_details[, "parameters"]
    est <- rep$parameters[rep$parameters$Label == para, "Value"]

   # Determine the parameter range
   if (profile_details$param_space == "relative") {
     range <- c(
       est + profile_details$low,
       est + profile_details$high
     )
   }
   if (profile_details$param_space == "multiplier") {
     range <- c(
       est - est * profile_details$low,
       est + est * profile_details$high
     )
   }
   if (profile_details$param_space == "real") {
     range <- c(
       profile_details$low,
       profile_details$high
     )
   }
   step_size <- profile_details$step_size

   # Create parameter vect from base down and the base up
   if (est != round_any(est, step_size, f = floor)) {
     low <- rev(seq(
       round_any(range[1], step_size, f = ceiling),
       round_any(est, step_size, f = floor), step_size
     ))
   } else {
     low <- rev(seq(
       round_any(range[1], step_size, f = ceiling),
       round_any(est, step_size, f = floor) - step_size, step_size
     ))
   }

   if (est != round_any(est, step_size, f = ceiling)) {
     high <- c(est, seq(round_any(est, step_size, f = ceiling), range[2], step_size))
   } else {
     high <- c(seq(round_any(est, step_size, f = ceiling), range[2], step_size))
   }

   vec <- c(low, high)
   cli::cli_inform(
     "Profiling over {para} across values of {vec}."
   )
  }
}
