#' Get default settings for profiles
#'
#' Create a data frame of parameters you want to profile over. The default
#' inputs for this function represent the typical parameters you would want
#' to profile over but the data frame can actually accommodate any viable
#' parameter in your Stock Synthesis model. Note that the defaults are, at a
#' minimum, the profiles needed in your assessment document if you are
#' presenting to the Pacific Fisheries Management Council.
#'
#' @details
#' The `param_space` argument indicates where the range of of the profile
#' parameter should be interpreted as relative to the base model estimate vs.
#' across a pre-specified range. An example is for R0 where the default setting
#' below indicates that the param_space is relative where the low bound for the
#' profile is set = base model log(R0) - 2 and high = base model log(R0) + 2.
#' The default range for M is set as a multiplier to explore a range of (M -
#' 0.40 * M) - (M + 0.40 * M) at a step size of 0.005. This range may be too
#' large (or small) with a step size too large (or too small) and should be
#' considered if the default settings are appropriate for your specific model.
#' The default setting for steepness is in 'real' space which means that the low
#' and high is in the same parameter space as the parameter. A user can select
#' any of the options for specifying a parameter range for any parameter.
#'
#' @param parameters A vector of character strings that specify the SS3
#'   parameter names that you want to conduct profiles for.
#' @param low,high A numeric vector specifying the low or high parameter bounds
#'   you want to use for the profile of each parameter in `parameters`.
#' @param step_size A numeric vector specifying the increments between the low
#'   and high bounds for each parameter in `parameters`. These values will be
#'   passed to `seq(from = low, to = high, by = step_size)`.
#' @param param_space A vector of character strings that specify the way in
#'   which you want the parameter bounds to be translated. The available options
#'   are, `"real"`, `"mulitplier"`, and `"relative"`.
#'   * `"real"` indicates bounds in the parameter space,
#'   * `"relative"` indicates how far to go from the base parameter, and
#'   * `"multiplier"` indicates that low and high bounds are set at x\% above
#'     and below the base parameter.
#' @param use_prior_like Deprecated: The use_prior_like input is no longer
#'   needed because r4ss now automatically plots the likelihood profile with and
#'   without any parameter prior likelihood contributions regardless of the
#'   setting in the user starter file.
#'
#' @return
#' A data frame with five columns,
#' * parameters,
#' * low,
#' * high,
#' * step_size, and
#' * param_space.
#' Where, there is one row for each parameter that will be profiled.
#' The default settings provide users with a good template that they can add
#' to or modify by giving some reasonable inputs for the default parameters
#' that are requested in the Terms of Reference.
#'
#' @author Chantel Wetzel & Kelli Johnson
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Define each parameter in real space
#' get_settings_profile(
#'   parameters = c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
#'   low = c(0.02, 0.25, 8),
#'   high = c(0.07, 1.0, 11),
#'   step_size = c(0.005, 0.05, 0.25),
#'   param_space = c("real", "real", "real")
#' )
#'
#' # Example 2: Run a profile for natural mortality one with the prior likelihood and one without
#' get_settings_profile(
#'   parameters = c("NatM_uniform_Fem_GP_1", "NatM_uniform_Fem_GP_1"),
#'   low = c(0.40, 0.40),
#'   high = c(0.40, 0.40),
#'   step_size = c(0.005, 0.005),
#'   param_space = c("multiplier", "multiplier")
#' )
#' }
#'
get_settings_profile <- function(parameters = c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
                                 low = c(0.40, 0.25, -2),
                                 high = c(0.40, 1.0, 2),
                                 step_size = c(0.01, 0.05, 0.25),
                                 param_space = c("multiplier", "real", "relative"),
                                 use_prior_like = lifecycle::deprecated()) {
  if (length(parameters) != length(low) |
    length(parameters) != length(high) |
    length(parameters) != length(step_size) |
    length(parameters) != length(param_space)) {
    cli::cli_abort(
      "Input vectors do match in length. There were {length(parameters)} parameters,
       {length(low)} lower bounds, {length(high)} high bounds, {length(step_size)}
        step sizes, and {length(param_space)} parameter spaces specified.")
  }

  if (lifecycle::is_present(use_prior_like)) {
    lifecycle::deprecate_warn(
      when = "1.1.2",
      what = "get_settings_profile(use_prior_like)"
    )
  }

  out <- data.frame(
    parameters = parameters,
    low = low,
    high = high,
    step_size = step_size,
    param_space = param_space
  )
  return(out)
}
