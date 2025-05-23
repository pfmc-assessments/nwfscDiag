#' Run MCMC using {adnuts} for SS3
#'
#' @param dir_wd A file path to the directory where the model files and the
#'   executable are saved. This can be relative to your current working
#'   directory or a full path. Your working directory will be changed while
#'   executing the function to be inside the location provided in this
#'   argument but upon exiting it will return to your current working
#'   directory.
#' @param model A string giving the name of the executable without an
#'   an extension. The extension is provided in `extension`. Do not include
#'   any path information, just the name of the executable. For example, the
#'   default is `"ss3"`.
#' @param extension A string giving the extension for the executable name
#'   provided in `model`. The default is `.exe`, which works on Windows
#'   machines.
#' @param iter A numeric value for the number of MCMC draws to do. Default is
#'   2000.
#' @param chains A numeric value for the number of chains to run. Default is 2.
#' @param interactive A logical, where `TRUE` will run
#'   [adnuts::launch_shinyadmb()]. The default is `FALSE`.
#' @param verbose A logical, specifying if information should be printed
#'   to the screen. The default is `FALSE` but using `TRUE` can be helpful
#'   when you are first running the function or when you have a very unstable
#'   model.
#'
#' @export
#' @author Cole M. Monnahan (cole.monnahan@noaa.gov; February 2021) and
#'   augmented by Kelli F. Johnson
#'
run_mcmc_diagnostics <- function(
    dir_wd,
    model = "ss3",
    extension = ".exe",
    iter = 2000,
    chains = 2,
    interactive = FALSE,
    verbose = FALSE) {
  # Set up and test model for running. This requires
  # pointing to a folder and executable. The folder needs to
  # contain all sufficient input files and assumes optimization
  # has occurred and produced all necessary outputs. Temporary
  # copies will be made in the working directory during execution
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  stopifnot(file.exists(dir_wd))
  setwd(dir_wd)

  # Check for exe
  exe_present <- file.exists(file.path(dir_wd, paste0(model, extension)))
  if (!exe_present) {
    cli::cli_abort(
      "An executable called {paste0(model, extension)} was not found in the directory. The
      executable is required to be in the folder for this function."
    )
  }

  # Check the recruitment option being used
  ctl <- r4ss::SS_readctl(file = file.path(dir_wd, "control.ss_new"))
  if (ctl$do_recdev != 2) {
    cli::cli_abort("Recruitment deviation option in the control file must be set to option 2 in order to avoid bias in MCMCs.")
  }
  # Turn off read the par file if necessary
  starter <- r4ss::SS_readstarter(file = file.path(dir_wd, "starter.ss"))
  if (starter$init_values_src == 1) {
    starter$init_values_src <- 0
    r4ss::SS_writestarter(
      mylist = starter,
      dir = dir_wd,
      overwrite = TRUE
    )
  }

  # Run the model if need be
  # if (!file.exists("Report.sso")) {
  #  system(paste(model, "-nohess"))
  # }

  # Regularization
  p <- "_mcmc"
  dir.create(p, showWarnings = FALSE)
  dir.create(file.path(p, "fits"))
  r4ss::copy_SS_inputs(
    dir.old = getwd(),
    dir.new = p,
    overwrite = TRUE,
    verbose = verbose
  )
  file.copy(
    paste0(model, extension),
    file.path(p, paste0(model, extension))
  )

  # optimize w/ -mcmc flag b/c of bias adjustment.
  r4ss::run(
    dir = p,
    exe = model,
    extras = "-nox -mcmc 100"
  )

  # Now test it works in parallel
  fit <- adnuts::sample_rwm(
    model = model,
    path = p,
    iter = iter,
    chains = chains
  )
  # This thin rate will lead to run time of ~60 mins below
  thin60min <- floor((60 * 60) / mean(fit$time.total))
  # ------------------------------------------------------------
  # Task 1: Run and demonstrate MCMC convergence diagnostics.
  chains <- parallel::detectCores() - 3

  # I recommend using 1000-2000 iterations, with first 10-25%
  # warmup. Start with thin=1, then increase thin rate until
  # convergence diagnostics passed (ESS>200 & Rhat<1.1).
  # printed to screen live!!
  thin <- thin60min # change this as needed
  iter <- iter * thin
  # Duration argument will stop after 40 minutes, only used
  # for the workshop to keep things organized
  # The below call was added to try to fix the test for a linux machine
  # but is did not fix the issue and caused an error for regular use.
  # Potential fix options: https://stackoverflow.com/questions/46503873/r-parallelisation-error-checkclustercl-not-a-valid-cluster
  # parallel::clusterEvalQ(chains, library(adnuts))
  fit <- adnuts::sample_rwm(
    model = model,
    path = p,
    iter = iter,
    warmup = floor(iter * 0.25),
    chains = chains,
    thin = thin,
    duration = 60
  )
  # Good idea to save the output, I recommend RDS format.
  saveRDS(fit, file = file.path(p, "fits", "mcmc.RDS"))
  # Marginal comparisons as multipage PDF for easy scrolling
  grDevices::pdf(
    file.path(p, "fits", "marginals.pdf"),
    onefile = TRUE, width = 7, height = 5
  )
  adnuts::plot_marginals(fit)
  grDevices::dev.off()

  # Key information from run. Including the two recommended
  # convergence diagnostics:
  if (verbose) summary(fit)

  # Interactive tools (must close out browser to regain console)
  if (interactive) adnuts::launch_shinyadmb(fit)

  # Extract posterior samples as a data.frame
  # ?extract_samples
  post <- adnuts::extract_samples(fit)

  # If more thinning is needed, increase and rerun, repeating.

  # ------------------------------------------------------------
  # Task 2: Model diagnostics using failed convergence
  # diagnostics. When the MLE and MCMC estimate completely
  # different things that is usually a parameterization issue.

  # Read in a longer previous run or use yours
  output_r4ss <- r4ss::SS_output(dir = p, covar = FALSE)
  names_good <- cbind(
    fit$par_names[-NROW(fit$par_names)],
    output_r4ss$parameters$Label[output_r4ss$parameters$Phase >= 0]
  )
  # Plot the slowest/fastest mixing parameters
  grDevices::png(file.path(p, "fits", "regularization-pairs-chains-slow6.png"))
  adnuts::pairs_admb(fit, pars = 1:6, order = "slow")
  grDevices::dev.off()
  grDevices::png(file.path(p, "fits", "regularization-pairs-hists-slow6.png"))
  adnuts::pairs_admb(fit, pars = 1:6, order = "slow", diag = "hist")
  grDevices::dev.off()
  grDevices::png(file.path(p, "fits", "regularization-pairs-hists-slow12.png"))
  adnuts::pairs_admb(fit, pars = 7:12, order = "slow", diag = "hist")
  grDevices::dev.off()
  grDevices::png(file.path(p, "fits", "regularization-pairs-hists-slow18.png"))
  adnuts::pairs_admb(fit, pars = 13:18, order = "slow", diag = "hist")
  grDevices::dev.off()
  grDevices::png(file.path(p, "fits", "regularization-pairs-hists-fast6.png"))
  adnuts::pairs_admb(fit, pars = 1:6, order = "fast")
  grDevices::dev.off()

  # Marginal MLE vs posterior
  # adnuts::plot_marginals(fit, pars=1:15)
  grDevices::png(
    file.path(p, "fits", "regularization-uncertainty-mle-v-mcmc.png")
  )
  x <- adnuts::plot_uncertainties(fit)
  grDevices::dev.off()
  grDevices::png(file.path(p, "fits", "regularization-marginals-worst-sd.png"))
  adnuts::plot_marginals(fit, pars = which.max(x$sd.post))
  grDevices::dev.off()

  # ------------------------------------------------------------
  # Task 3: Posterior extraction for inference. mceval can be run
  # from command line b/c post-warmup samples from all chains
  # were merged into main folder, so any mceval output files
  # contain all this information
}
