#' Create output files with jitter quantities
#'
#' Tables generated to visualize results
#'
#'
#' @template mydir
#' @param output List of model output created by [run_jitter()].
#' @template model_settings
#'
#' @author Chantel Wetzel
#' @return
#' Nothing is explicitly returned from [get_jitter_quants()].
#'
#'
#' @export

get_jitter_quants <- function(mydir, model_settings, output) {
  jitter_dir <- output[["plotdir"]]
  like <- output[["like"]]
  est <- output[["est"]]
  profilesummary <- output[["profilesummary"]]

  outputs <- output[["profilemodels"]]
  quants <- lapply(outputs, "[[", "derived_quants")
  status <- sapply(sapply(outputs, "[[", "parameters", simplify = FALSE), "[[", "Status")
  bounds <- apply(status, 2, function(x) rownames(outputs[[1]][["parameters"]])[x %in% c("LO", "HI")])
  out <- data.frame(
    "run" = gsub("replist", "", names(outputs)),
    "likelihood" = sapply(sapply(outputs, "[[", "likelihoods_used", simplify = FALSE), "[", 1, 1),
    "gradient" = sapply(outputs, "[[", "maximum_gradient_component"),
    "SB0" = sapply(quants, "[[", "SSB_Virgin", "Value"),
    "SBfinal" = sapply(quants, "[[", paste0("SSB_", profilesummary[["endyrs"]][1]), "Value"),
    "Nparsonbounds" = apply(status, 2, function(x) sum(x %in% c("LO", "HI"))),
    "Lowest NLL" = ifelse(min(like) == like, "Best Fit", 0),
    stringsAsFactors = FALSE
  )

  # Write a md file to be included in a stock assessment document
  # Text was pirated from @chantelwetzel-noaa's 2021 dover assessment
  utils::write.csv(
    x = data.frame(
      caption = paste(
        sep = "",
        "Model convergence was in part based on starting the minimization process ",
        "from dispersed values of the maximum likelihood estimates to determine if the ",
        "estimation routine results in a smaller likelihood.",
        "Starting parameters were jittered using the built-in functionality of ",
        "Stock Synthesis, where you specify a jitter fraction.",
        "Here we used a jitter fraction of ",
        round(model_settings[["jitter_fraction"]], 2), " and the jittering was repeated ",
        xfun::numbers_to_words(model_settings[["Njitter"]]), " times.",
        "A better, i.e., lower negative log-likelihood, fit was ",
        dplyr::if_else(
          sum(like - est < 0) == 0,
          true = "not found",
          false = paste0("found for ", xfun::numbers_to_words(sum(like - est < 0)), " fits")
        ),
        "Through the jittering analysis performed here and ",
        "the estimation of likelihood profiles, ",
        "we are confident that the base model as presented represents the ",
        "best fit to the data given the assumptions made."
      ),
      alt_caption = "Comparison of the negative log-likelihood across jitter runs",
      label = c("jitter", "jitter-zoomed"),
      filein = file.path("..", jitter_dir, c("jitter.png", "jitter_zoomed.png"))
    ),
    file = file.path(jitter_dir, "jitterfigures4doc.csv"),
    row.names = FALSE
  )

  # write tables
  utils::write.csv(x = table(unlist(bounds)), file = file.path(jitter_dir, "jitter_parsonbounds.csv"), row.names = FALSE)
  utils::write.csv(x = out, file = file.path(jitter_dir, "jitter_results.csv"), row.names = FALSE)
}
