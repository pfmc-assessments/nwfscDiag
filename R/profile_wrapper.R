#' Run [r4ss::profile()] based on `model_settings`
#'
#' Setting up the specifications, running the profile using [r4ss::profile()],
#' and generating figures and tables can be tedious, error prone, and time
#' consuming. Thus, `profile_wrapper()` aims to further decrease the work
#' needed to generate a profile that is easily included in a assessment report
#' for the Pacific Fisheries Management Council. See the See Also section for
#' information on a workflow to use this function.
#'
#' @seealso
#' The following functions interact with `profile_wrapper()`:
#' * [get_settings()]: populates a list of settings for `model_settings`
#' * [run_diagnostics()]: calls `profile_wrapper()`
#' * [r4ss::profile()]: the workhorse of `profile_wrapper()` that does the
#'   parameter profiles
#'
#' @template mydir
#' @template model_settings
#'
#' @author Chantel Wetzel and Ian Taylor.
#' @return
#' Nothing is explicitly returned from `profile_wrapper()`.
#' The following objects are saved to the disk:
#' * `*_profile_output.Rdata`
#' * `*_trajectories_*` from [r4ss::SSplotComparisons()]
#' * `piner_panel_*.png`
#' * `parameter_panel_*.png`
#' * `run_diag_warning.txt`
#' * a copy of the control file saved to `model_settings$newctlfile`
#' * `backup_oldctlfile.ss`
#' * `backup_ss.par`
#' @export

profile_wrapper <- function(mydir, model_settings) {
  N <- nrow(model_settings$profile_details)

  for (aa in 1:N) {
    para <- model_settings[["profile_details"]][["parameters"]][aa]
    profile_settings <- model_settings
    profile_settings[["profile_details"]] <- profile_settings[["profile_details"]][aa, ]
    output <- run_profile(
      mydir = mydir,
      model_settings = profile_settings,
      para = para
    )

    get_summary(
      mydir = output[["mydir"]],
      para = para,
      vec = output[["profilesummary"]][["pars"]] |>
        dplyr::filter(Label == para) |>
        dplyr::select(dplyr::starts_with("rep")) |>
        as.vector(),
      profilemodels = output[["profilemodels"]],
      profilesummary = output[["profilesummary"]]
    )

    get_param_values(
      mydir = output[["mydir"]],
      para = para,
      vec = output[["vec"]],
      summary = output[["profilesummary"]]
    )

    plot_profile(
      mydir = output[["mydir"]],
      para = para,
      rep = output[["rep"]],
      profilesummary = output[["profilesummary"]]
    )
    cli::cli_inform("Finished profile of {para}.")
  }
}
