#' Create plots for retrospective runs
#'
#'
#' @template mydir
#' @template model_settings
#' @param output List of model output created by [run_retro()].
#'
#' @author Chantel Wetzel
#' @return
#' Nothing is explicitly returned from [plot_retro()].
#' The following objects are saved to the disk.
#'
#' * A set of figures from [r4ss::SSplotComparisons]
#' * `retrofigures4doc.csv` for use with `sa4ss::add_figure` to add retro peels of
#'   spawning stock biomass (_SSB_) and fraction unfished,
#'   complete with captions and alternative text.
#'
#' @export

plot_retro <- function(mydir, model_settings, output) {
  retro_dir <- output[["plotdir"]]
  endyrvec <- output[["endyrvec"]]
  retroSummary <- output[["retroSummary"]]
  rhosall <- output[["rhosall"]]
  rhos <- output[["rhos"]]

  # Make figures, copy over two figures with ones that have Mohn's rho values
  r4ss::SSplotComparisons(
    summaryoutput = retroSummary,
    endyrvec = endyrvec,
    legendlabels = c(
      "Base Model",
      sprintf(
        "Data %.0f year%s",
        model_settings[["retro_yrs"]],
        ifelse(abs(model_settings[["retro_yrs"]]) == 1, "", "s")
      )
    ),
    btarg = model_settings[["btarg"]],
    minbthresh = model_settings[["minbthresh"]],
    ylimAdj = 1.2,
    plotdir = retro_dir,
    legendloc = "topright",
    print = TRUE,
    plot = FALSE,
    pdf = FALSE,
    verbose = model_settings[["verbose"]]
  )
  savedplotinfo <- mapply(
    FUN = r4ss::SSplotComparisons,
    MoreArgs = list(
      summaryoutput = retroSummary,
      endyrvec = endyrvec,
      legendloc = "topleft",
      plotdir = retro_dir,
      ylimAdj = 1.2,
      btarg = model_settings[["btarg"]],
      minbthresh = model_settings[["minbthresh"]],
      print = TRUE, plot = FALSE, pdf = FALSE,
      verbose = model_settings[["verbose"]]
    ),
    subplot = c(8, 10),
    legendlabels = lapply(
      c("AFSC_Hurtado_F", "AFSC_Hurtado_Rec"),
      function(x) {
        c(
          "Base Model",
          sprintf(
            "Data %.0f year%s (Revised Mohn's rho %.2f)",
            model_settings[["retro_yrs"]],
            ifelse(abs(model_settings[["retro_yrs"]]) == 1, "", "s"),
            rhosall[rownames(rhosall) == x, ]
          )
        )
      }
    )
  )

  r4ss::SSplotComparisons(
    summaryoutput = retroSummary,
    endyrvec = endyrvec,
    legendlabels = c(
      "Base Model",
      sprintf(
        "Data %.0f year%s",
        model_settings[["retro_yrs"]],
        ifelse(abs(model_settings[["retro_yrs"]]) == 1, "", "s")
      )
    ),
    btarg = model_settings[["btarg"]],
    minbthresh = model_settings[["minbthresh"]],
    subplot = c(2, 4),
    ylimAdj = 1.2,
    plotdir = retro_dir,
    legendloc = "topright",
    print = TRUE,
    plot = FALSE,
    pdf = FALSE,
    verbose = model_settings[["verbose"]]
  )
  savedplotinfo <- mapply(
    FUN = r4ss::SSplotComparisons,
    MoreArgs = list(
      summaryoutput = retroSummary,
      endyrvec = endyrvec,
      legendloc = "topright",
      ylimAdj = 1.2,
      plotdir = retro_dir,
      btarg = model_settings[["btarg"]],
      minbthresh = model_settings[["minbthresh"]],
      print = TRUE, plot = FALSE, pdf = FALSE,
      verbose = model_settings[["verbose"]]
    ),
    subplot = c(2, 4),
    legendlabels = lapply(
      c("AFSC_Hurtado_SSB", "AFSC_Hurtado_Bratio"),
      function(x) {
        c(
          "Base Model",
          sprintf(
            "Data %.0f year%s (Revised Mohn's rho %.2f)",
            model_settings[["retro_yrs"]],
            ifelse(abs(model_settings[["retro_yrs"]]) == 1, "", "s"),
            rhosall[rownames(rhosall) == x, ]
          )
        )
      }
    )
  )

  label <- retroSummary[["SpawnOutputLabels"]][1]
  n <- ncol(retroSummary[["SpawnBio"]]) - 2
  years <- retroSummary[["startyrs"]][1]:retroSummary[["endyrs"]][1] + 1
  denom <- paste0("model", 1:n)
  base <- as.symbol("model1")
  sb <- retroSummary[["SpawnBio"]] |>
    dplyr::filter(Yr %in% years) |>
    dplyr::mutate(dplyr::across(
      # Finds the columns that match "model1", "model2", ...
      .cols = dplyr::matches("model[0-9]+$"),
      # model1 is the reference for all model comparisons
      .fns = \(x) 100 * x / model1 - 100,
      # renames the output columns per_diff_model1, per_diff_model2, ...
      .names = "per_diff_{.col}"
    ))
  sb[["Reference_Point"]] <- label
  bratio <- retroSummary[["Bratio"]] |>
    dplyr::filter(Yr %in% years) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::matches("model[0-9]+$"),
      .fns = \(x) 100 * x / model1 - 100,
      .names = "per_diff_{.col}"
    ))
  bratio[["Reference_Point"]] <- "Fraction Unfished"
  f <- retroSummary[["Fvalue"]] |>
    dplyr::filter(Yr %in% years) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::matches("model[0-9]+$"),
      .fns = \(x) 100 * x / model1 - 100,
      .names = "per_diff_{.col}"
    ))
  f[["Reference_Point"]] <- "F"
  rec <- retroSummary[["recruits"]] |>
    dplyr::filter(Yr %in% years) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::matches("model[0-9]+$"),
      .fns = \(x) 100 * x / model1 - 100,
      .names = "per_diff_{.col}"
    ))
  rec[["Reference_Point"]] <- "Recruits"

  col_names <- c("Yr", "Reference_Point", paste0("per_diff_model", 1:n))
  df <- rbind(
    sb[, colnames(sb) %in% col_names],
    bratio[, colnames(bratio) %in% col_names],
    f[, colnames(f) %in% col_names],
    rec[, colnames(rec) %in% col_names]
  ) |>
    tidyr::pivot_longer(
      cols = starts_with("per_diff_model"),
      names_to = "model",
      values_to = "diff"
    )

  df_out <- NULL
  y <- years
  for (a in 1:n) {
    col_name <- paste0("per_diff_model", a)
    df_out <- rbind(df_out, df[df[["Yr"]] %in% y & df[["model"]] %in% col_name, ])
    if (a == 1) {
      df_out[["model"]][df_out[["model"]] == col_name] <- "Base Model"
    } else {
      df_out[["model"]][df_out[["model"]] == col_name] <- paste0("Retro -", a - 1)
    }
    y <- y - 1
  }
  colnames(df_out)[colnames(df_out) == "model"] <- "Run"
  leg_order <- c("Base Model", paste0("Retro -", 1:(length(endyrvec) - 1)))
  df_out[["Run"]] <- factor(df_out[["Run"]], levels = leg_order)
  xrange <- c(ifelse(min(df_out[["Yr"]]) < 1980, 1980, min(df_out[["Yr"]])), max(df_out[["Yr"]]))
  find <- df_out |>
    dplyr::filter(Yr >= xrange[1]) |>
    dplyr::summarize(
      bound = abs(max(diff))
    )
  yrange <- c(-1 * find[["bound"]] - 5, find[["bound"]] + 5)

  ggplot2::ggplot(df_out, ggplot2::aes(x = Yr, y = diff, col = Run)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    # ylim(yrange) +
    ggplot2::scale_x_continuous(limits = xrange, expand = c(0, 0)) +
    ggplot2::ylab("% Differece from Base Model") +
    ggplot2::xlab("Year") +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::facet_wrap("Reference_Point", scales = "free_y")
  ggplot2::ggsave(filename = file.path(retro_dir, "retro_percent_difference_4_panel.png"), width = 10, height = 12)

  sub_out <- df_out[df_out[["Reference_Point"]] != "Recruits", ]
  find <- sub_out |>
    dplyr::filter(Yr >= xrange[1]) |>
    dplyr::summarize(
      bound = abs(max(diff))
    )
  yrange <- c(-1 * find[["bound"]] - 5, find[["bound"]] + 5)

  ggplot2::ggplot(sub_out, ggplot2::aes(x = Yr, y = diff, col = Run)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::ylim(yrange) +
    ggplot2::scale_x_continuous(limits = xrange, expand = c(0, 0)) +
    ggplot2::ylab("% Differece from Base Model") +
    ggplot2::xlab("Year") +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::facet_wrap("Reference_Point", ncol = 1, nrow = 3)
  ggplot2::ggsave(filename = file.path(retro_dir, "retro_percent_difference_3_panel.png"), width = 10, height = 12)

  endyrvec_long <- (min(endyrvec) - 5):max(endyrvec)
  pngfun(wd = retro_dir, file = "recruitment_retrospective_squid.png", h = 7, w = 7)
  r4ss::SSplotRetroRecruits(
    retroSummary = retroSummary,
    endyrvec = endyrvec,
    cohorts = endyrvec_long,
    main = ""
  )
  dev.off()
}
