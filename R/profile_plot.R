#' Generate likelihood profiles plots
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model_settings input of all settings created using the get_settings function
#' @param rep base model outpus
#' @param vec vector of value that were profiled over
#' @param para parameter name SS control.ss_new expected parameter name
#' @param profilesummary output from [r4ss::SSsummarize()]
#'
#' @author Chantel Wetzel.
#' @export

profile_plot <- function(mydir, model_settings, rep, vec, para, profilesummary){

  label = ifelse(para == "SR_LN(R0)", expression(log(italic(R)[0])),
  	      ifelse(para == "NatM_p_1_Fem_GP_1", "Natural Mortality (female)",
  	      ifelse(para == "NatM_p_1_Mal_GP_1", "Natural Mortality (male)",
  	      ifelse(para == "SR_BH_steep", "Steepness (h)",
  	      para))))

  get = ifelse(para == "SR_LN(R0)", "R0", para)

  n <- 1:profilesummary$n

  ymax1 = max(profilesummary$likelihoods[1, n]) - min(profilesummary$likelihoods[1, n])
  ymax2 = max(max(profilesummary$likelihoods[8,  n]) - min(profilesummary$likelihoods[8, n]),
		      max(profilesummary$likelihoods[9, n])  - min(profilesummary$likelihoods[9, n]),
		      max(profilesummary$likelihoods[4, n])  - min(profilesummary$likelihoods[4, n]) )

  png(file.path(mydir, paste0("piner_panel_", para, ".png")), height = 7, width = 7, units = "in", res = 300)
  par(mfrow = c(2,2))
  SSplotProfile(summaryoutput = profilesummary, main = "Changes in total likelihood", profile.string = get, 
                profile.label = label, ymax = ymax1)
  if(ymax1 < 15) { abline(h = 1.92, lty = 3, col = 'red') }

  PinerPlot (summaryoutput = profilesummary, plot = TRUE, print = FALSE, component = "Length_like",
             main = "Length-composition likelihoods", profile.string = get, profile.label = label,
             ylab = "Change in -log-likelihood", legendloc = "topright", ymax = ymax2)
  
  PinerPlot (summaryoutput = profilesummary, plot = TRUE, print = FALSE, component = "Age_like",
             main = "Age-composition likelihoods", profile.string = get, profile.label = label,
             ylab = "Change in -log-likelihood", legendloc = "topright", ymax = ymax2)
  
  PinerPlot (summaryoutput = profilesummary, plot = TRUE, print = FALSE, component = "Surv_like",
             main = "Survey likelihoods", profile.string = get, profile.label = label,
             ylab = "Change in -log-likelihood", legendloc = "topright", ymax = ymax2)
  dev.off()

  maxyr <- min(profilesummary$endyrs)
  minyr <- max(profilesummary$startyrs)

  est      <- rep$parameters[rep$parameters$Label == para, "Value", 2] 
  sb0_est  <- rep$derived_quants[rep$derived_quants$Label == "SSB_Virgin", "Value"]
  sbf_est  <- rep$derived_quants[rep$derived_quants$Label == paste0("SSB_", maxyr), "Value"]
  depl_est <- rep$derived_quants[rep$derived_quants$Label == paste0("Bratio_", maxyr), "Value"]


  x <- as.numeric(profilesummary$pars[profilesummary$pars$Label == para, n]) 
  like <- as.numeric(profilesummary$likelihoods[profilesummary$likelihoods$Label == "TOTAL", n] - rep$likelihoods_used[1,1])
  ylike<- c(0, max(like))
  sb0  <- as.numeric(profilesummary$SpawnBio[na.omit(profilesummary$SpawnBio$Label) == "SSB_Virgin", n])
  sbf  <- as.numeric(profilesummary$SpawnBio[na.omit(profilesummary$SpawnBio$Yr) == maxyr, n])
  depl <- as.numeric(profilesummary$Bratio[na.omit(profilesummary$Bratio$Yr) == maxyr, n])
  
  png(file.path(mydir, paste0("parameter_panel_", para, ".png")), height = 7, width = 7, units = "in", res = 300)
  par(mfrow = c(2,2))
  # parameter vs. likelihood
  plot(x, like, type = "l", lwd = 2, xlab =  label, ylab ="Change in -log-likelihood", ylim = ylike)
  abline(h = 0, lty = 2, col = 'black')
  if(max(ylike) < 15) { abline(h =  1.92, lty = 3, col = 'red') }
  points(est, 0, pch = 21, col = "black", bg = "blue", cex = 1.5)

  # parameter vs. final depletion
  plot(x, depl, type = "l", lwd = 2, xlab = label, ylab = "Fraction of unfished", ylim = c(0, 1.2))
  points(est, depl_est, pch = 21, col = "black", bg = "blue", cex = 1.5)
  #abline(h = c(0.25,0.4), lty = c(2, 2), col = c("red", "darkgreen"))

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

  find <- which(est == vec)
  modelnames <- paste(get, "=", vec)
  modelnames[find] = paste("Base:", modelnames[find])
  SSplotComparisons(profilesummary, 
  					legendlabels = modelnames, 
  					plotdir = mydir, subplots = c(1, 3), 
  					pdf = FALSE, print = TRUE, 
  					filenameprefix = paste0(para, "_trajectories_"))

}
