#' Run [r4ss::jitter] based on `model_settings`
#'
#' Code to run jitters for a model
#' Output will be saved in an Rdata object called "jitter_output"
#' Plots and tables generated to visualize results
#'
#' @seealso The following functions interact with `jitter_wrapper`:
#' * [run_diagnostics]: calls `jitter_wrapper`
#'
#' @template mydir
#' @template model_settings
#'
#' @author Chantel Wetzel
#' @return
#' Nothing is explicitly returned from `jitter_wrapper`.
#'
#'
#' @export

jitter_wrapper <- function(mydir, model_settings) {
  output <- run_jitter(
    mydir = mydir,
    model_settings = model_settings
  )
  plot_jitter(
    mydir = mydir,
    model_settings = model_settings,
    output = output
  )
  get_jitter_quants(
    mydir = mydir,
    model_settings = model_settings,
    output = output
  )
  cli::cli_inform("Finished jitters.")
}
