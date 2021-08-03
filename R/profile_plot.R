#' Generate likelihood profile plots
#'
#' Create plots to be included in presentations and documents that
#' summarize a likelihood profile.
#'
#' @template mydir
#' @param rep A list of model output as returned by [r4ss::SS_output()].
#' @param para A character value that matches a parameter name as used in the
#' SS control.ss_new parameter names.
#' The name will be cleaned up for plotting purposes internally within the function.
#' For example, `SR_BH_steep` becomes "Steepness (_h_)".
#' @param profilesummary Output from [r4ss::SSsummarize()].
#' Ensure that the results are ordered according to the parameter of interest.
#' Otherwise, the profile plots will not be smooth and lines will zig-zag back and forth.
#'
#' @author Chantel Wetzel.
#' @export
#' @seealso [profile_wrapper] and [rerun_profile_vals] call `profile_plot`.

profile_plot <- function(mydir, rep, para, profilesummary){

  label <- ifelse(para == "SR_LN(R0)", expression(log(italic(R)[0])),
          ifelse(para %in% c("NatM_p_1_Fem_GP_1", "NatM_uniform_Fem_GP_1"), "Natural Mortality (female)",
          ifelse(para %in% c("NatM_p_1_Mal_GP_1", "NatM_uniform_Mal_GP_1"), "Natural Mortality (male)",
          ifelse(para == "SR_BH_steep", expression(Steepness~(italic(h))),
          para))))

  get = ifelse(para == "SR_LN(R0)", "R0", para)

  if(para %in% c("SR_LN(R0)", "NatM_p_1_Fem_GP_1", "NatM_p_1_Mal_GP_1", "SR_BH_steep")){
    exact = FALSE
  } else {
    exact = TRUE
  }  

  n <- 1:profilesummary$n
  
  like_comp  <- unique(profilesummary$likelihoods_by_fleet$Label[
                  c(-grep("_lambda", profilesummary$likelihoods_by_fleet$Label),
                    -grep("_N_use", profilesummary$likelihoods_by_fleet$Label),
                    -grep("_N_skip", profilesummary$likelihoods_by_fleet$Label))])
  ii <- which(profilesummary$likelihoods_by_fleet$Label %in% like_comp)
  check <- aggregate(ALL~Label, profilesummary$likelihoods_by_fleet[ii,], FUN = sum)
  use <- check[which(check$ALL != 0),"Label"]
  # If present remove the likes that we don't typically show
  use <- use[which(!use %in% c("Disc_like", "Catch_like", "mnwt_like"))]

  tot_plot <- length(use) 
  if(tot_plot == 1) { panel <- c(2, 1) }
  if(tot_plot != 1 & tot_plot <= 3) { panel <- c(3, 1) }
  if(tot_plot >= 3) { panel <- c(2, 2) }
  if(tot_plot >= 4) { panel <- c(3, 2) }

  # Determine the y-axis for the profile plot for all data types together
  ymax1 = max(profilesummary$likelihoods[1, n]) - min(profilesummary$likelihoods[1, n])
  if(ymax1 > 70) { ymax1 = 70}
  if(ymax1 <  5) { ymax1 = 5}

  # Determine the y-axis for the piner profile plots by each data type
  lab.row = ncol(profilesummary$likelihoods)
  ymax2 = max(apply(profilesummary$likelihoods[-1,-lab.row],1,max) - 
              apply(profilesummary$likelihoods[-1,-lab.row],1,min))
  if(ymax2 > 70) { ymax2 = 70}
  if(ymax2 <  5) { ymax2 = 5}

  pngfun(wd = mydir, file = paste0("piner_panel_", para, ".png"), h= 7, w = 7)
  par(mfrow = panel)
  r4ss::SSplotProfile(summaryoutput = profilesummary, main = "Changes in total likelihood", profile.string = get, 
                profile.label = label, ymax = ymax1, exact = exact)
  abline(h = 1.92, lty = 3, col = 'red') 

  if ("Length_like" %in% use){
  r4ss::PinerPlot (summaryoutput = profilesummary, plot = TRUE, print = FALSE, component = "Length_like",
             main = "Length-composition likelihoods", profile.string = get, profile.label = label,
             exact = exact, ylab = "Change in -log-likelihood", legendloc = "topright", ymax = ymax2)
  }
  
  if ("Age_like" %in% use){
  r4ss::PinerPlot (summaryoutput = profilesummary, plot = TRUE, print = FALSE, component = "Age_like",
             main = "Age-composition likelihoods", profile.string = get, profile.label = label,
             exact = exact, ylab = "Change in -log-likelihood", legendloc = "topright", ymax = ymax2)
  }
  
  if ("Surv_like" %in% use){
  r4ss::PinerPlot (summaryoutput = profilesummary, plot = TRUE, print = FALSE, component = "Surv_like",
             main = "Survey likelihoods", profile.string = get, profile.label = label,
             exact = exact, ylab = "Change in -log-likelihood", legendloc = "topright", ymax = ymax2)
  }

  if ("Init_equ_like" %in% use){
  r4ss::PinerPlot (summaryoutput = profilesummary, plot = TRUE, print = FALSE, component = "Init_equ_like",
             main = "Initial equilibrium likelihoods", profile.string = get, profile.label = label,
             exact = exact, ylab = "Change in -log-likelihood", legendloc = "topright", ymax = ymax2)
  }

  dev.off()

  maxyr <- min(profilesummary$endyrs)
  minyr <- max(profilesummary$startyrs)

  est      <- rep$parameters[rep$parameters$Label == para, "Value", 2] 
  sb0_est  <- rep$derived_quants[rep$derived_quants$Label == "SSB_Virgin", "Value"]
  sbf_est  <- rep$derived_quants[rep$derived_quants$Label == paste0("SSB_", maxyr), "Value"]
  depl_est <- rep$derived_quants[rep$derived_quants$Label == paste0("Bratio_", maxyr), "Value"]

  x <- as.numeric(profilesummary$pars[profilesummary$pars$Label == para, n]) 
  like <- as.numeric(profilesummary$likelihoods[profilesummary$likelihoods$Label == "TOTAL", n] - rep$likelihoods_used[1,1])
  ylike<- c(min(like) + ifelse(min(like) != 0, -0.5, 0), max(like))
  sb0  <- as.numeric(profilesummary$SpawnBio[na.omit(profilesummary$SpawnBio$Label) == "SSB_Virgin", n])
  sbf  <- as.numeric(profilesummary$SpawnBio[na.omit(profilesummary$SpawnBio$Yr) == maxyr, n])
  depl <- as.numeric(profilesummary$Bratio[na.omit(profilesummary$Bratio$Yr) == maxyr, n])

  # Get the relative management targets - only grab the first element since the targets should be the same 
  btarg  <- as.numeric(profilesummary$btargs[1]) 
  thresh <- ifelse(btarg == 0.40, 0.25, 0.125)

  pngfun(wd = mydir, file = paste0("parameter_panel_", para, ".png"), h = 7, w = 7)
  par(mfrow = c(2,2), mar = c(4,4,2,2), oma = c(1,1,1,1))
  # parameter vs. likelihood
  plot(x, like, type = "l", lwd = 2, xlab =  label, ylab ="Change in -log-likelihood", ylim = ylike)
  abline(h = 0, lty = 2, col = 'black')
  if(max(ylike) < 40) { 
    abline(h =  1.92, lty = 3, col = 'red') 
    abline(h = -1.92, lty = 3, col = 'red') }
  points(est, 0, pch = 21, col = "black", bg = "blue", cex = 1.5)

  # parameter vs. final depletion
  plot(x, depl, type = "l", lwd = 2, xlab = label, ylab = "Fraction of unfished", ylim = c(0, 1.2))
  points(est, depl_est, pch = 21, col = "black", bg = "blue", cex = 1.5)
  abline(h = c(thresh, btarg), lty = c(2, 2), col = c("red", "darkgreen"))
  legend("bottomright", legend = c("Management target", "Minimum stock size threshold"),
    lty = 2, col = c('red', 'darkgreen'), bty = 'n')

  # parameter vs. SB0
  plot(x, sb0, type = "l", lwd = 2, xlab = label, ylab = expression(SB[0]), ylim=c(0, max(sb0)))
  points(est, sb0_est, pch = 21, col = "black", bg = "blue", cex = 1.5)

  # parameter vs. SBfinal
  plot(x, sbf, type = "l", lwd = 2, xlab = label, ylab = expression(SB[final]), ylim=c(0, max(sbf)))
  points(est, sbf_est, pch = 21, col = "black", bg = "blue", cex = 1.5)

  dev.off()

  # Create the sb and depl trajectories plot
  # Figure out what the base model parameter is in order to label that in the plot
  get = ifelse(para == "SR_LN(R0)", "log(R0)",
  	    ifelse(para == "NatM_p_1_Fem_GP_1", "M (f)",
  	    ifelse(para == "NatM_p_1_Mal_GP_1", "M (m)",
  	    ifelse(para == "SR_BH_steep", "h",
  	    para))))

  r4ss::SSplotComparisons(profilesummary,
    legendlabels = sprintf(
      paste0("%s = %.", max(nchar(gsub("0+$|[0-9]+\\.", "", x[-which(est == x)]))), "f%s"),
      get,
      x,
      ifelse(est == x, " (base)", "")
    ),
    ylimAdj = 1.15,
    plotdir = mydir, subplots = c(1, 3),
    pdf = FALSE, print = TRUE, plot = FALSE,
    filenameprefix = paste0(para, "_trajectories_")
  )

}
