#' Run [r4ss::profile()] based on `model_settings`
#'
#' Setting up the specifications, running the profile using [r4ss::profile()],
#' and generating figures and tables can be tedious, error prone, and time
#' consuming. Thus, `profile_wrapper()` aims to further decrease the work
#' needed to generate a profile that is easily included in a assessment report
#' for the Pacific Fisheries Management Council. See the See Also section for
#' information on a workflow to use this function.
#'
#' @seealso
#' The following functions interact with `profile_wrapper()`:
#' * [get_settings()]: populates a list of settings for `model_settings`
#' * [run_diagnostics()]: calls `profile_wrapper()`
#' * [r4ss::profile()]: the workhorse of `profile_wrapper()` that does the
#'   parameter profiles
#'
#' @template mydir
#' @template model_settings
#'
#' @author Chantel Wetzel and Ian Taylor.
#' @return
#' Nothing is explicitly returned from `profile_wrapper()`.
#' The following objects are saved to the disk:
#' * `*_profile_output.Rdata`
#' * `*_trajectories_*` from [r4ss::SSplotComparisons()]
#' * `piner_panel_*.png`
#' * `parameter_panel_*.png`
#' * `run_diag_warning.txt`
#' * a copy of the control file saved to `model_settings$newctlfile`
#' * `backup_oldctlfile.ss`
#' * `backup_ss.par`
#' @export

profile_wrapper <- function(mydir, model_settings) {

  # Add the round_any function from the plyr package to avoid conflicts between
  # plyr and dplyr.
  round_any <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }

  N <- nrow(model_settings$profile_details)

  for (aa in 1:N) {
    para <- model_settings$profile_details$parameters[aa]
    # Create a profile folder with the same naming structure as the base model
    # Add a label to show if prior was used or not
    profile_dir <- file.path(mydir, paste0(model_settings$base_name, "_profile_", para))
    dir.create(profile_dir, showWarnings = FALSE)

    # Check for existing files and delete
    if (model_settings$remove_files & length(list.files(profile_dir)) != 0) {
      remove <- list.files(profile_dir)
      file.remove(file.path(profile_dir, remove))
    }

    all_files <- list.files(file.path(mydir, model_settings$base_name))
    utils::capture.output(file.copy(
      from = file.path(mydir, model_settings$base_name, all_files),
      to = profile_dir, overwrite = TRUE
    ), file = "run_diag_warning.txt")
    cli::cli_inform("Running profile for {para}.")

    # check for whether oldctlfile exists
    if (!file.exists(file.path(profile_dir, model_settings$oldctlfile))) {
      # if the oldctlfile is control.ss_new, and doesn't exist,
      # run the model to create it
      if (model_settings$oldctlfile == "control.ss_new") {
        if (model_settings$verbose) {
          message("running model to get control.ss_new file")
        }
        r4ss::run(
          dir = profile_dir,
          exe = model_settings[["exe"]],
          extras = model_settings[["extras"]],
          skipfinished = FALSE,
          verbose = model_settings[["verbose"]]
        )
      } else {
        cli::cli_abort("Can not find {model_settings$oldctlfile}")
      }
    }

    # Use the SS_parlines function to ensure that the input parameter can be found
    check_para <- r4ss::SS_parlines(
      ctlfile =  model_settings$oldctlfile,
      dir = profile_dir,
      verbose = FALSE,
      version = model_settings$version,
      active = FALSE
    )$Label == para

    if (sum(check_para) == 0) {
      print(para)
      cli::cli_abort("The input profile_custom does not match a parameter in the file
        {model_settings$oldctlfile}")
    }

    # Copy oldctlfile to newctlfile before modifying it
    file.copy(file.path(profile_dir, model_settings$oldctlfile),
      file.path(profile_dir, model_settings$newctlfile))

    # Change the control file name in the starter file
    starter <- r4ss::SS_readstarter(file = file.path(profile_dir, "starter.ss"))
    starter$ctlfile <- model_settings$newctlfile
    starter$init_values_src <- model_settings$init_values_src
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

    # backup original control.ss_new file for use in second half of profile
    file.copy(file.path(profile_dir, model_settings$oldctlfile),
      file.path(profile_dir, "backup_oldctlfile.ss"),
      overwrite = model_settings$overwrite)
    # backup original par file for use in second half of profile
    # if usepar = TRUE
    file.copy(file.path(profile_dir, "ss.par"),
      file.path(profile_dir, "backup_ss.par"),
      overwrite = model_settings$overwrite)

    # loop over down, then up
    for (iprofile in 1:2) {
      whichruns <- which(vec %in% if(iprofile == 1){low} else {high})
      if (!is.null(model_settings$whichruns)) {
        whichruns <- intersect(model_settings$whichruns, whichruns)
      }
      if (iprofile == 2) {
        # copy backup back to use in second half of profile
        file.copy(file.path(profile_dir, "backup_oldctlfile.ss"),
          file.path(profile_dir, model_settings$oldctlfile))
        # copy backup back to use in second half of profile
        file.copy(file.path(profile_dir, "backup_ss.par"),
          file.path(profile_dir, "ss.par"),
          overwrite = model_settings$overwrite)
      }
      profile <- r4ss::profile(
        dir = profile_dir,
        oldctlfile = model_settings$oldctlfile,
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
        whichruns = whichruns, # values set above
        prior_check = model_settings$prior_check,
        exe = model_settings$exe,
        verbose = model_settings$verbose,
        extras = model_settings$extras
      )
    }

    # Save the output and the summary
    name <- paste0("profile_", para)
    vec_unordered <- vec
    vec <- vec[num]

    profilemodels <- r4ss::SSgetoutput(dirvec = profile_dir, keyvec = num)
    profilesummary <- r4ss::SSsummarize(biglist = profilemodels)
    if(!is.null(model_settings$btarg)){
      profilesummary$btarg <- model_settings$btarg
      profilesummary$minbthresh <- model_settings$minbthresh
    }
    profilesummary$subplots <- model_settings$subplots

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

    cli::cli_inform("Finished profile of {para}.")
  } # end parameter loop
}
