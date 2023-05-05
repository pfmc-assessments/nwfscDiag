#' Run [r4ss::profile] based on `model_settings`
#'
#' Generate likelihood profiles
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#'
#'
#' @seealso The following functions interact with `profile_wrapper`:
#' * [run_diagnostics]: calls `profile_wrapper`
#' * [r4ss::profile]: the workhorse of `profile_wrapper` that does the parameter profiles
#'
#'
#' @template mydir
#' @template model_settings
#'
#' @author Chantel Wetzel.
#' @return
#' Nothing is explicitly returned from `profile_wrapper`.
#' The following objects are saved to the disk.
#' @export

profile_wrapper <- function(mydir, model_settings) {

  # Add the round_any function from the plyr package to avoid conflicts between
  # plyr and dplyr.
  round_any <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }

  check_exe <- paste0(model_settings$exe, ".exe")
  # check whether exe is in directory
  if (!check_exe %in% list.files(file.path(mydir, model_settings$base_name))) {
    stop("Executable not found in ", file.path(mydir, model_settings$base_name))
  }

  N <- nrow(model_settings$profile_details)

  for (aa in 1:N) {
    para <- model_settings$profile_details$parameters[aa]
    prior_used <- model_settings$profile_details$use_prior_like[aa]
    # Create a profile folder with the same naming structure as the base model
    # Add a label to show if prior was used or not
    profile_dir <- file.path(mydir, paste0(model_settings$base_name, "_profile_", para, "_prior_like_", prior_used))
    dir.create(profile_dir, showWarnings = FALSE)

    # Check for existing files and delete
    if (model_settings$remove_files & length(list.files(profile_dir)) != 0) {
      remove <- list.files(profile_dir)
      file.remove(file.path(profile_dir, remove))
    }

    all_files <- list.files(file.path(mydir, model_settings$base_name))
    capture.output(file.copy(
      from = file.path(mydir, model_settings$base_name, all_files),
      to = profile_dir, overwrite = TRUE
    ), file = "run_diag_warning.txt")
    message(paste0("Running profile for ", para, "."))

    # Copy the control file to run from the copy
    if (!file.exists(file.path(profile_dir, "control.ss_new"))) {
      orig_dir <- getwd()
      setwd(profile_dir)
      r4ss::run(
        dir = profile_dir,
        exe = model_settings$exe,
        extras = model_settings$extras
      )
      setwd(orig_dir)
    }

    # Use the SS_parlines funtion to ensure that the input parameter can be found
    check_para <- r4ss::SS_parlines(
      ctlfile = "control.ss_new",
      dir = profile_dir,
      verbose = FALSE,
      version = model_settings$version,
      active = FALSE
    )$Label == para

    if (sum(check_para) == 0) {
      print(para)
      stop(paste0("The input profile_custom does not match a parameter in the control.ss_new file."))
    }

    file.copy(file.path(profile_dir, "control.ss_new"), file.path(profile_dir, model_settings$newctlfile))
    # Change the control file name in the starter file
    starter <- r4ss::SS_readstarter(file = file.path(profile_dir, "starter.ss"))
    starter$ctlfile <- "control_modified.ss"
    starter$init_values_src <- model_settings$init_values_src
    # make sure the prior likelihood is calculated for non-estimated quantities
    starter$prior_like <- prior_used
    r4ss::SS_writestarter(mylist = starter, dir = profile_dir, overwrite = TRUE)

    # Read in the base model
    rep <- r4ss::SS_output(file.path(mydir, model_settings$base_name),
      covar = FALSE,
      printstats = FALSE, verbose = FALSE
    )
    est <- rep$parameters[rep$parameters$Label == para, "Value"]

    # Determine the parameter range
    if (model_settings$profile_details$param_space[aa] == "relative") {
      range <- c(
        est + model_settings$profile_details$low[aa],
        est + model_settings$profile_details$high[aa]
      )
    }
    if (model_settings$profile_details$param_space[aa] == "multiplier") {
      range <- c(
        est - est * model_settings$profile_details$low[aa],
        est + est * model_settings$profile_details$high[aa]
      )
    }
    if (model_settings$profile_details$param_space[aa] == "real") {
      range <- c(
        model_settings$profile_details$low[aa],
        model_settings$profile_details$high[aa]
      )
    }
    step_size <- model_settings$profile_details$step_size[aa]

    # Create parameter vect from base down and the base up
    if (est != round_any(est, step_size, f = floor)) {
      low <- rev(seq(
        round_any(range[1], step_size, f = ceiling),
        round_any(est, step_size, f = floor), step_size
      ))
    } else {
      low <- rev(seq(
        round_any(range[1], step_size, f = ceiling),
        round_any(est, step_size, f = floor) - step_size, step_size
      ))
    }

    if (est != round_any(est, step_size, f = ceiling)) {
      high <- c(est, seq(round_any(est, step_size, f = ceiling), range[2], step_size))
    } else {
      high <- c(seq(round_any(est, step_size, f = ceiling), range[2], step_size))
    }

    vec <- c(low, high)
    num <- sort(vec, index.return = TRUE)$ix

    profile <- r4ss::profile(
      dir = profile_dir,
      oldctlfile = "control.ss_new",
      newctlfile = model_settings$newctlfile,
      linenum = model_settings$linenum,
      string = para,
      profilevec = vec,
      usepar = model_settings$usepar,
      globalpar = model_settings$globalpar,
      parlinenum = model_settings$parlinenum,
      parstring = model_settings$parstring,
      saveoutput = model_settings$saveoutput,
      overwrite = model_settings$overwrite,
      whichruns = model_settings$whichruns,
      prior_check = model_settings$prior_check,
      read_like = model_settings$read_like,
      exe = model_settings$exe,
      verbose = model_settings$verbose,
      extras = model_settings$extras
    )

    # Save the output and the summary
    name <- paste0("profile_", para)
    vec_unordered <- vec
    vec <- vec[num]

    profilemodels <- r4ss::SSgetoutput(dirvec = profile_dir, keyvec = num)
    profilesummary <- r4ss::SSsummarize(biglist = profilemodels)
    if(!is.null(model_settings$btarg)){
      profilesummary$btargs <- model_settings$btarg
      profilesummary$minbthreshs <- model_settings$minbthresh
    }

    profile_output <- list()
    profile_output$mydir <- profile_dir
    profile_output$para <- para
    profile_output$name <- paste0("profile_", para)
    profile_output$vec <- vec[num]
    profile_output$model_settings <- model_settings
    profile_output$profilemodels <- profilemodels
    profile_output$profilesummary <- profilesummary
    profile_output$rep <- rep
    profile_output$vec_unordered <- vec
    profile_output$num <- num

    save(
      profile_dir,
      para,
      name,
      vec,
      vec_unordered,
      model_settings,
      profilemodels,
      profilesummary,
      rep,
      num,
      file = file.path(profile_dir, paste0(para, "_profile_output.Rdata"))
    )

    nwfscDiag::get_summary(
      mydir = profile_dir,
      name = paste0("profile_", para),
      para = para,
      # vec = vec[num],
      vec = profilesummary$pars %>%
        dplyr::filter(Label == para) %>%
        dplyr::select(dplyr::starts_with("rep")) %>%
        as.vector(),
      profilemodels = profilemodels,
      profilesummary = profilesummary
    )

    nwfscDiag::profile_plot(
      mydir = profile_dir,
      para = para,
      rep = rep,
      profilesummary = profilesummary
    )

    message("Finished profile of ", para, ".")
  } # end parameter loop
}
