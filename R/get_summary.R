#' Generate results from likelihood profiles
#'
#' Used by [profile_wrapper()] to write summary statistics to the disk.
#'
#' @template mydir
#' @param name Identify if the csv file show jitter, retro, or profile results.
#' @param para A character string specifying the SS3 parameter name that the
#'   profile pertains to.
#' @param vec A numeric vector specifying the parameter values that the
#'   profile covers.
#' @param profilemodels An object returned from [r4ss::SSgetoutput()].
#' @param profilesummary An object returned from [r4ss::SSsummarize()].
#'
#' @author Chantel Wetzel & Kelli Johnson
#' @export

get_summary <- function(mydir, para, vec, name, profilemodels, profilesummary) {

  # Need to identify a way to determine if a model estimates male growth parameters as offsets from females

  # get output
  outputs <- profilemodels
  quants <- lapply(outputs, "[[", "derived_quants")
  status <- sapply(sapply(outputs, "[[", "parameters", simplify = FALSE), "[[", "Status")
  bounds <- apply(status, 2, function(x) rownames(outputs[[1]]$parameters)[x %in% c("LO", "HI")])

  out <- data.frame(
    "run" = gsub("replist", "", names(outputs)),
    "profile_parameter" = para,
    "parameter_value" = as.numeric(vec),
    "likelihood" = sapply(sapply(outputs, "[[", "likelihoods_used", simplify = FALSE), "[", 1, 1),
    "gradient" = sapply(outputs, "[[", "maximum_gradient_component"),
    "SB0" = sapply(quants, "[[", "SSB_Virgin", "Value"),
    "SBfinal" = sapply(quants, "[[", paste0("SSB_", outputs[[1]]$endyr + 1), "Value"),
    "Deplfinal" = sapply(quants, "[[", paste0("Bratio_", outputs[[1]]$endyr + 1), "Value"),
    # "Fmsy" = sapply(quants, "[[", "annF_MSY", "Value"),
    "Nparsonbounds" = apply(status, 2, function(x) sum(x %in% c("LO", "HI"))),
    stringsAsFactors = FALSE
  )

  # write tables
  utils::write.csv(x = table(unlist(bounds)), file = file.path(mydir, paste0(name, "_parsonbounds.csv")), row.names = FALSE)

  utils::write.csv(x = out, file = file.path(mydir, paste0(name, "_results.csv")), row.names = FALSE)

  get_param_values(
    mydir = mydir,
    para = para,
    vec = vec,
    summary = profilesummary)

  return()
}
