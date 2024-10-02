#' Run [r4ss::retro] based on `model_settings`
#'
#' Create a folder containing retrospective runs for a given base-model folder.
#' Runs are created using settings specified in `model_settings` and
#' is formatted for inclusion in a document.
#'
#' @seealso The following functions interact with `retro_wrapper`:
#' * [run_diagnostics]: calls `retro_wrapper`
#' * [r4ss::retro]: the workhorse of `retro_wrapper` that does the peels
#'
#' @template mydir
#' @template model_settings
#'
#' @author Chantel Wetzel
#' @return
#' Nothing is explicitly returned from `retro_wrapper`.
#' The following objects are saved to the disk.
#'
#' * `mohnsrho.csv` with the following columns:
#'   * type: the type of Mohn's rho
#'     * [Mohn (1999)](https://academic.oup.com/icesjms/article/56/4/473/624639),
#'     * Woods Hole Mohn's rho [(Legault 2009)](https://archive.nefmc.org/tech/council_mtg_docs/Sept%202009/Herring/Doc%209_Retro%20Working%20Group%20Report.pdf) used by the [Northeast Fisheries Science Center (NEFSC)](https://www.fisheries.noaa.gov/about/northeast-fisheries-science-center), and
#'     * [Hurtado-Ferro et al. (2015)](https://doi.org/10.1093/icesjms/fsu198) used by the [Alaska Fisheries Science Center (AFSC)](https://www.fisheries.noaa.gov/about/alaska-fisheries-science-center)
#'   * Quantity: the stock assessment quantity of interest
#'   * values: the Mohn's rho values
#' * A set of figures from [r4ss::SSplotComparisons]
#' * `retrofigures4doc.csv` for use with `sa4ss::add_figure` to add retro peels of
#'   spawning stock biomass (_SSB_) and fraction unfished,
#'   complete with captions and alternative text.
#'
#'   `apply(utils::read.csv(file.path("..", paste0(mod_loc, "_retro"), "retrofigures4doc.csv")), 1, function(x) do.call(sa4ss::add_figure, as.list(x)))`
#'
#' * `mohnsrho.tex` for use with `sa4ss::read_child()`
#'   inside of an environment with `results = "asis"`
#'   to include a table of Mohn's rho values in a document.
#'
#'   `sa4ss::read_child(file.path(paste0(params$model, "_retro"), "mohnsrho.tex"))`
#'
#' * `retro_output.Rdata` with a list of R objects.
#'
#' @export

retro_wrapper <- function(mydir, model_settings) {
  output <- run_retro(
    mydir = mydir,
    model_settings = model_settings
  )
  plot_retro(
    mydir = mydir,
    model_settings = model_settings,
    output = output
  )
  get_retro_quants(
    mydir = mydir,
    model_settings = model_settings,
    output = output
  )
  cli::cli_inform("Finished retrospective runs.")
}
