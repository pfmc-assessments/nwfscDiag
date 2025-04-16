#' Check model estimated total catch to the GEMM estimates. This function
#' should only be used to compare estimates for coastwide models.
#'
#' @param  replist List of model objects created by `r4ss::SS_output()`.
#' @param  common_name Common name of the species in the GEMM database.
#' @param  gemm_data A data frame of GEMM data from `nwfscSurvey::pull_gemm()`.
#'   If this argument is left blank, the function will pull the GEMM data
#'   and evaluate mortality on a coastwide basis.  However, if your model
#'   is not coastwide and the GEMM data can be split out to match the model
#'   area (e.g., assessment split north and south of 40 10 N. latitude) this
#'   filtered data frame can be passed to the function via this argument and
#'   the total mortality comparisons will be done relative to that data object.
#'   The default value is NULL.
#' @param  dir Directory to save figure. If no directory is provided the
#'   figure is not saved. If a `dir` is specified a figure of the model
#'   estimated total mortality compared to the GEMM total mortality, a
#'   figure of total mortality across all years for the model, a rda of
#'   all model years landed and catch estimates by fleet, and a rda of the
#'   model estimated catch compared to the GEMM from 2003+. Default is NULL.
#' @param  add_name String that can be added to the saved figure names.
#' @param  verbose A logical that specifies if you want to print messages and
#'   warnings to the console. The default is `TRUE`.
#'
#' @author Chantel Wetzel
#' @export
#'
compare_model_gemm_catch <- function(
    replist,
    common_name,
    gemm_data = NULL,
    dir = NULL,
    add_name = NULL,
    verbose = TRUE) {
  nwfscSurvey::check_dir(dir = dir, verbose = verbose)
  if (!is.null(add_name)) { add_name <- paste0("_", add_name)}
  # Check for column name in the CATCH table based on SS3 version
  if (any("kill_bio" %in% colnames(replist[["catch"]]))) {
    colnames(replist[["catch"]])[colnames(replist[["catch"]]) == "kill_bio"] <- "dead_bio"
  }
  catch <- replist[["catch"]] |>
    dplyr::select(Fleet_Name, Yr, sel_bio, dead_bio, ret_bio) |>
    dplyr::rename(year = Yr) |>
    dplyr::mutate(
      discard = sel_bio - ret_bio,
      dead_discard = dead_bio - ret_bio,
      discard_rate = (sel_bio - ret_bio) / sel_bio
    ) |>
    dplyr::rename(
      landings = ret_bio,
      catch = dead_bio
    ) |>
    dplyr::select(-sel_bio)

  if (!is.null(dir)) {
    save(
      catch,
      file = file.path(dir, paste0("model_estimated_catch", add_name, ".rda"))
    )
  }

  if (is.null(gemm_data)){
    gemm_data <- nwfscSurvey::pull_gemm(common_name = common_name, dir = dir)
  }
  gemm <- gemm_data |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      gemm_dead_discard = sum(total_discard_with_mort_rates_applied_mt),
      gemm_total_catch = sum(total_discard_with_mort_rates_applied_and_landings_mt)
    )

  if (dim(gemm)[1] == 0) {
    if (verbose) {
      cli::cli_abort("{common_name} was not found in the GEMM, check common name.")
    }
  }
  catch_gemm <- catch |>
    dplyr::filter(year %in% unique(gemm$year))

  gg1 <- ggplot2::ggplot() +
    ggplot2::geom_bar(stat = "identity", data = catch_gemm, ggplot2::aes(x = year, y = catch, fill = Fleet_Name), alpha = 0.75) +
    ggplot2::geom_line(data = gemm, ggplot2::aes(x = year, y = gemm_total_catch), color = "black", linewidth = 2) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::ylab("Total Catch (mt)") +
    ggplot2::xlab("Year") +
    ggplot2::theme_bw()

  if (!is.null(dir)){
    ggplot2::ggsave(
      gg1,
      filename = file.path(dir, paste0("model_gemm_catch_comparison", add_name, ".png"))
    )
  }

  gg2 <- ggplot2::ggplot() +
    ggplot2::geom_bar(stat = "identity", data = catch_gemm, ggplot2::aes(x = year, y = dead_discard, fill = Fleet_Name), alpha = 0.75) +
    ggplot2::geom_line(data = gemm, ggplot2::aes(x = year, y = gemm_dead_discard), color = "black", linewidth = 2) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::ylab("Dead Discard (mt)") +
    ggplot2::xlab("Year") +
    ggplot2::theme_bw()

  if (!is.null(dir)){
    ggplot2::ggsave(
      gg2,
      filename = file.path(dir, paste0("model_gemm_dead_discard_comparison", add_name, ".png"))
    )
  } else {
    gg2
  }

  out <- catch_gemm |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      catch = sum(catch)
    ) |>
    dplyr::left_join(
      gemm
    ) |>
    dplyr::mutate(
      model_minus_gemm = round(catch - gemm_total_catch, 3)
    ) |>
    dplyr::select(-gemm_dead_discard) |>
    dplyr::rename(
      model_catch = catch,
      gemm_catch = gemm_total_catch
    )

  if (!is.null(dir)) {
    save(
      catch,
      file = file.path(dir, paste0("model_gemm_catch_comparison", add_name, ".rda"))
    )
  }
  return(out)
}
