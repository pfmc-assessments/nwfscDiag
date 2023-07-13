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

retro_wrapper <- function(mydir,  model_settings) {

  if(!file.exists(file.path(mydir, model_settings$base_name, "Report.sso"))) {
    message("There is no Report.sso file in the base model directory", file.path(mydir, model_settings$base_name))
    stop()
  }

	# Create a jitter folder with the same naming structure as the base model
	retro_dir <- file.path(mydir, paste0(model_settings$base_name, "_retro"))
  dir.create(retro_dir, showWarnings = FALSE)
  all_files = list.files(file.path(mydir, model_settings$base_name)) 
  ignore <- file.copy(
    from = file.path(mydir, model_settings$base_name, all_files),
    to = retro_dir,
    overwrite = TRUE
  )
  message("Running retrospectives.")

  r4ss::retro(
    dir = retro_dir, 
    oldsubdir = model_settings$oldsubdir, 
    newsubdir = model_settings$newsubdir,  	
    years = model_settings$retro_yrs,
    overwrite = model_settings$overwrite,
    exe = model_settings$exe, 
    extras = model_settings$extras,
    show_in_console = model_settings$show_in_console
  )

  ignore <- file.remove(from = file.path(retro_dir, all_files))

  runs <- list()
  for(aa in 1:(length(model_settings$retro_yrs) + 1)) {
  	if (aa == 1) { 
      runs[[aa]] <- r4ss::SS_output(dir = file.path(mydir, model_settings$base_name), verbose = FALSE, printstats = FALSE)
  	} else {
  		tmp = file.path(retro_dir, model_settings$newsubdir, paste0("retro", model_settings$retro_yrs[aa-1]))
  		runs[[aa]] <- r4ss::SS_output(dir = tmp, verbose = FALSE, printstats = FALSE)
  	}
  }

  retroSummary <- r4ss::SSsummarize(biglist = runs, verbose = FALSE)
	endyrvec <- c(retroSummary$endyrs[1], retroSummary$endyrs[1] + model_settings$retro_yrs)

  # Calculate Mohn's rho
  rhosall <- mapply(
    r4ss::SSmohnsrho,
    summaryoutput = lapply(
      seq_along(runs)[-1],
      function(x) r4ss::SSsummarize(runs[1:x], verbose = FALSE)
    ),
    endyrvec = mapply(seq,from=endyrvec[1], to= endyrvec[-1])
  )

  rhos <- rhosall %>%
    data.frame %>%
    dplyr::select(values = NCOL(rhosall)) %>%
    tibble::rownames_to_column("ind") %>%
    dplyr::mutate(
      ind = gsub("\\.all$", "", ind),
      Quantity = gsub("[A-Za-z_]+_([A-Za-z]+$)|(^[A-Za-z]+$)", "\\1\\2", ind),
      Quantity = gsub("Bratio", "Fraction unfished", Quantity),
      Quantity = gsub("Rec", "Recruitment", Quantity),
      ind = gsub("_[A-Za-z]+$|^[A-Za-z]+$", "", ind),
      ind = gsub("^$", "Mohn", ind),
      ind = gsub("WoodHole", "NEFSC", ind),
      ind = gsub("_Hurtado", "", ind),
    ) %>%
    dplyr::rename(type = "ind") %>%
    dplyr::select(type, Quantity, values)
  utils::write.csv(
    x = as.matrix(rhos),
    row.names = FALSE,
    file = file.path(retro_dir, "mohnsrho.csv")
  )

  retro_output <- list()
  retro_output$plotdir <- retro_dir
  retro_output$endyrvec <- endyrvec
  retro_output$retroSummary <- retroSummary
  retro_output$model_settings <- model_settings
  retro_output$rhosall <- rhosall
  retro_output$rhos <- rhos

  save(
    retro_dir, 
    endyrvec,
    retroSummary,
    model_settings,
    rhosall,
    rhos,
    file = file.path(retro_dir, "retro_output.Rdata")
  )

  # Make figures, copy over two figures with ones that have Mohn's rho values
	r4ss::SSplotComparisons(
    summaryoutput = retroSummary,
    endyrvec = endyrvec,
    legendlabels = c(
      "Base Model",
      sprintf("Data %.0f year%s",
        model_settings$retro_yrs,
        ifelse(abs(model_settings$retro_yrs) == 1, "", "s")
      )
    ),
    btarg = model_settings$btarg, 
    minbthresh = model_settings$minbthresh,
    plotdir = retro_dir,
    legendloc = "topright",
    print = TRUE,
    plot = FALSE,
    pdf = FALSE
  )
  savedplotinfo <- mapply(
    FUN = r4ss::SSplotComparisons,
    MoreArgs = list(
      summaryoutput = retroSummary,
      endyrvec = endyrvec,
      legendloc = "topleft",
      plotdir = retro_dir,
      btarg = model_settings$btarg, 
      minbthresh = model_settings$minbthresh,
      print = TRUE, plot = FALSE, pdf = FALSE
    ),
    subplot = c(2, 4, 8, 10),
    legendlabels = lapply(
      c("AFSC_Hurtado_SSB", "AFSC_Hurtado_Bratio", "AFSC_Hurtado_F", "AFSC_Hurtado_Rec"),
      function(x) {
      c(
        "Base Model",
        sprintf("Data %.0f year%s (Mohn's rho %.2f)",
          model_settings$retro_yrs,
          ifelse(abs(model_settings$retro_yrs) == 1, "", "s"),
          rhosall[rownames(rhosall) == x, ]
        )
      )
    })
  )

  label <- ifelse(retroSummary$SpawnOutputUnits[1] == "biomass", 
    "Spawning Biomass", "Spawning Output")
  n <- ncol(retroSummary$SpawnBio) - 2
  years <- retroSummary$startyr[1]:retroSummary$endyr[1] + 1
  denom <- paste0("model", 1:n)
  base <- as.symbol("model1")
  sb <- retroSummary$SpawnBio %>%
      subset(Yr %in% years) %>%
      mutate_at(vars(denom), list(per_diff = ~100*./model1 - 100)) 
  sb$Reference_Point <- label
  bratio <- retroSummary$Bratio %>%
      subset(Yr %in% years) %>%
      mutate_at(vars(denom), list(per_diff = ~100*./model1 - 100)) 
  bratio$Reference_Point <- "Fraction Unfished"
  f <- retroSummary$Fvalue %>%
      subset(Yr %in% years) %>%
      mutate_at(vars(denom), list(per_diff = ~100*./model1 - 100)) 
  f$Reference_Point <- "F"

  col_names <- c("Yr", "Reference_Point", paste0("model", 1:n, "_per_diff"))
  df <- rbind(sb[ , colnames(sb) %in% col_names], 
              bratio[, colnames(bratio) %in% col_names],
              f[ , colnames(f) %in% col_names]) %>%
      tidyr::pivot_longer(cols = starts_with("model"), names_to = "model", values_to = "diff")

  df_out <- NULL
  y <- years
  for (a in 1:n){
    col_name <- paste0("model", a, "_per_diff")
    df_out <- rbind(df_out, df[df$Yr %in% y & df$model %in% col_name, ])
    if (a == 1){
      df_out$model[df_out$model == col_name] <- "Base Model"
    } else {
      df_out$model[df_out$model == col_name] <- paste0("Retro -", a-1)
    }
    y <- y - 1
  } 
  colnames(df_out)[colnames(df_out) == "model"] <- "Run"
  xrange <- c(ifelse(min(df_out$Yr) < 1980, 1980, min(df_out$Yr)), max(df_out$Yr))
  yrange <- c(-1 * max(abs(df_out$diff)) - 5, max(abs(df_out$diff)) + 5)

  ggplot() +
      geom_line(data = df_out, aes(x = Yr, y = diff, col = Run), linewidth = 2) +
      ylim(yrange) + 
      scale_x_continuous(limits = xrange, expand = c(0,0)) + 
      ylab("% Differece from Base Model") +
      xlab("Year") +
      scale_colour_viridis_d() +
      theme_bw(base_size = 15) + 
      facet_wrap("Reference_Point", nrow = 3, ncol = 1)
  ggsave(filename = retro_dir, width = 7, height = 12)
  
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

  # Make table for document with caption
  rhos %>%
    tidyr::spread(key = "type", value = "values") %>%
    dplyr::mutate(
      Quantity = gsub("(^[FSB]+$)", "\\\\emph{\\1}", Quantity),
    ) %>%
    kableExtra::kbl(
      format = "latex", booktabs = TRUE, digits = 2, longtable = TRUE,
      label = "RetroMohnsrho",
      escape = FALSE,
      caption = paste(
        "The magnitude of retrospective pattern",
        "(Mohn's rho; Mohn, 1999) given the removal of",
        xfun::numbers_to_words(max(abs(model_settings$retro_yr))),
        "years of data for",
        "fishing intensity (\\emph{F}),",
        "fraction unfished (Figure \\ref{fig:RetroFractionunfished}),",
        " recruitment, and",
        "spawning stock biomass (\\emph{SSB}; Figure \\ref{fig:RetroSsb}).",
        "Columns are",
        "a derivation of Mohn's rho (Hurtado-Ferro et al. 2015) used by the Alaska Fisheries Science Center (AFSC),",
        "as originally described in Mohn (1999),",
        "and a derivation of Mohn's rho (Woods Hole Mohn's rho; Legault 2009) used by the Northeast Fisheries Science Center (NEFSC)."
      )
    ) %>%
    kableExtra::kable_classic(full_width = FALSE) %>%
    kableExtra::save_kable(
      file = file.path(retro_dir, "mohnsrho.tex")
    )

	message("Finished retrospective runs.")
}
