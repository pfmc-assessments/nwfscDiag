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
#'   `sa4ss::read_child(file.path(paste0(params[["model"]], "_retro"), "mohnsrho.tex"))`
#'
#' * `retro_output.Rdata` with a list of R objects.
#'
#' @export

run_retro <- function(mydir, model_settings) {
  if (!file.exists(file.path(mydir, model_settings[["base_name"]], "Report.sso"))) {
    base <- model_settings[["base_name"]]
    cli::cli_abort("There is no Report.sso file in the base model directory {file.path(mydir, base}")
  }

  # Create a jitter folder with the same naming structure as the base model
  retro_dir <- file.path(mydir, paste0(model_settings[["base_name"]], "_retro_", length(model_settings[["retro_yrs"]]), "_yr_peel"))
  dir.create(retro_dir, showWarnings = FALSE)
  all_files <- list.files(file.path(mydir, model_settings[["base_name"]]))
  ignore <- file.copy(
    from = file.path(mydir, model_settings[["base_name"]], all_files),
    to = retro_dir,
    overwrite = TRUE
  )
  cli::cli_inform("Running retrospectives.")

  r4ss::retro(
    dir = retro_dir,
    oldsubdir = model_settings[["oldsubdir"]],
    newsubdir = model_settings[["newsubdir"]],
    years = model_settings[["retro_yrs"]],
    overwrite = model_settings[["overwrite"]],
    exe = model_settings[["exe"]],
    extras = model_settings[["extras"]],
    show_in_console = model_settings[["show_in_console"]],
    verbose = FALSE
  )

  ignore <- file.remove(from = file.path(retro_dir, all_files))

  runs <- list()
  for (aa in 1:(length(model_settings[["retro_yrs"]]) + 1)) {
    if (aa == 1) {
      runs[[aa]] <- r4ss::SS_output(dir = file.path(mydir, model_settings[["base_name"]]), verbose = FALSE, printstats = FALSE)
    } else {
      tmp <- file.path(retro_dir, model_settings[["newsubdir"]], paste0("retro", model_settings[["retro_yrs"]][aa - 1]))
      runs[[aa]] <- r4ss::SS_output(dir = tmp, verbose = FALSE, printstats = FALSE)
    }
  }

  retroSummary <- r4ss::SSsummarize(biglist = runs, verbose = FALSE)
  endyrvec <- c(retroSummary[["endyrs"]][1], retroSummary[["endyrs"]][1] + model_settings[["retro_yrs"]])

  # Calculate Mohn's rho
  rhosall <- mapply(
    r4ss::SSmohnsrho,
    summaryoutput = lapply(
      seq_along(runs)[-1],
      function(x) r4ss::SSsummarize(runs[1:x], verbose = FALSE)
    ),
    verbose = FALSE,
    endyrvec = mapply(seq, from = endyrvec[1], to = endyrvec[-1])
  )

  rhos <- data.frame(rhosall) |>
    dplyr::select(values = NCOL(rhosall)) |>
    tibble::rownames_to_column("ind") |>
    dplyr::mutate(
      ind = gsub("\\.all$", "", ind),
      Quantity = gsub("[A-Za-z_]+_([A-Za-z]+$)|(^[A-Za-z]+$)", "\\1\\2", ind),
      Quantity = gsub("Bratio", "Fraction unfished", Quantity),
      Quantity = gsub("Rec", "Recruitment", Quantity),
      ind = gsub("_[A-Za-z]+$|^[A-Za-z]+$", "", ind),
      ind = gsub("^$", "Mohn", ind),
      ind = gsub("WoodHole", "NEFSC", ind),
      ind = gsub("_Hurtado", "", ind),
    ) |>
    dplyr::rename(type = "ind") |>
    dplyr::select(type, Quantity, values)
  utils::write.csv(
    x = as.matrix(rhos),
    row.names = FALSE,
    file = file.path(retro_dir, "mohnsrho.csv")
  )

  retro_output <- list()
  retro_output[["plotdir"]] <- retro_dir
  retro_output[["endyrvec"]] <- endyrvec
  retro_output[["retroSummary"]] <- retroSummary
  retro_output[["model_settings"]] <- model_settings
  retro_output[["rhosall"]] <- rhosall
  retro_output[["rhos"]] <- rhos

  save(
    retro_dir,
    endyrvec,
    retroSummary,
    model_settings,
    rhosall,
    rhos,
    file = file.path(retro_dir, "retro_output.Rdata")
  )

  return(retro_output)
}
