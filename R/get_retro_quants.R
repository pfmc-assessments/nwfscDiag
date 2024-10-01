#' Get model values across retrospective runs
#'
#' @template mydir
#' @template model_settings
#' @param output List of model output created by [run_retro()].
#'
#' @author Chantel Wetzel
#' @return
#' Nothing is explicitly returned from [get_retro_quants()].
#' The following objects are saved to the disk.
#'
#' * `mohnsrho.csv` with the following columns:
#'   * type: the type of Mohn's rho
#'     * [Mohn (1999)](https://academic.oup.com/icesjms/article/56/4/473/624639),
#'     * Woods Hole Mohn's rho [(Legault 2009)](https://archive.nefmc.org/tech/council_mtg_docs/Sept%202009/Herring/Doc%209_Retro%20Working%20Group%20Report.pdf) used by the [Northeast Fisheries Science Center (NEFSC)](https://www.fisheries.noaa.gov/about/northeast-fisheries-science-center), and
#'     * [Hurtado-Ferro et al. (2015)](https://doi.org/10.1093/icesjms/fsu198) used by the [Alaska Fisheries Science Center (AFSC)](https://www.fisheries.noaa.gov/about/alaska-fisheries-science-center)
#'   * Quantity: the stock assessment quantity of interest
#'   * values: the Mohn's rho values
#'
#'   `apply(utils::read.csv(file.path("..", paste0(mod_loc, "_retro"), "retrofigures4doc.csv")), 1, function(x) do.call(sa4ss::add_figure, as.list(x)))`
#'
#' * `mohnsrho.tex` for use with `sa4ss::read_child()`
#'   inside of an environment with `results = "asis"`
#'   to include a table of Mohn's rho values in a document.
#'
#'   `sa4ss::read_child(file.path(paste0(params$model, "_retro"), "mohnsrho.tex"))`
#'
#'
#' @export

get_retro_quants <- function(mydir,  model_settings, output) {
  retro_dir <- output[["plotdir"]]
  endyrvec <- output[["endyrvec"]]
  retroSummary <- output[["retroSummary"]]
  rhosall <- output[["rhosall"]]
  rhos <- output[["rhos"]]

  get_param_values(
    mydir = retro_dir,
    para = "retro",
    vec = c("Base Model", paste0("Retro -", 1:(length(endyrvec)-1))),
    summary = retroSummary
  )

  utils::write.csv(
    x = data.frame(
      caption = paste(
        "Retrospective patterns for",
        c("spawning stock biomass (\\emph{SSB})", "fraction unfished"),
        "when up to", xfun::numbers_to_words(max(abs(model_settings$retro_yr))),
        "years of data were removed from the base model.",
        "Mohn's rho (Mohn, 1999) values were",
        "recalculated for each peel given the removal of another year of data.",
        "See Table \\ref{tab:RetroMohnsrho} for other derivations of Mohn's rho."
      ),
      alt_caption = sprintf("Each successive peel of data led to a Mohn's rho of %s for %s.",
                            lapply(c("SSB", "Bratio"), function(x) {
                              knitr::combine_words(sprintf("%.2f", (rhosall[rownames(rhosall) == x, ])))
                            }),
                            c("SSB", "fraction unfished")
      ),
      label = c("RetroSsb", "RetroFractionunfished"),
      filein = file.path("..", retro_dir, c("compare2_spawnbio_uncertainty.png", "compare4_Bratio_uncertainty.png"))
    ),
    file = file.path(retro_dir, "retrofigures4doc.csv"),
    row.names = FALSE
  )
}
