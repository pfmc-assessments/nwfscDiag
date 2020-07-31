#' Generate likelihood profiles plots
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model_settings input of all settings created using the get_settings function
#' @param para parameter option to profile, options = "profile_m", "profile_h", "profile_r0", profile_custom
#' @author Chantel Wetzel.
#' @export

profile_plot <- function(mydir, model_settings, para, profilesummary){

  label = ifelse(para == "SR_LN(R0)", expression(log(italic(R)[0]))
  	      ifelse(para == "NatM_p_1_Fem_GP_1", "Natural Mortality (female)",
  	      ifelse(para == "NatM_p_1_Mal_GP_1", "Natural Mortality (male)",
  	      ifelse(para == "steep", "Steepness (h)",
  	      para))))

  png(file.path(mydir, "piner_panel_", para, ".png"), height = 7, width = 7, units = "in", res = 300)
  par(mfrow = c(2,2))
  SSplotProfile(summaryoutput = profilesummary, 
  			    main = "Changes in total likelihood", 
  			    profile.string = para, 
                profile.label = label,
                ymax = y.max)
  
  PinerPlot (summaryoutput = profilesummary,
             plot=TRUE, print=FALSE,
             component = "Length_like",
             main = "Changes in length-composition likelihoods",
             profile.string = para,
             profile.label = label,
             ylab = "Change in -log-likelihood",
            legendloc = "topright", 
            ymax = y.max2)
  
  PinerPlot (summaryoutput = profilesummary,
             plot = TRUE, print = FALSE,
             component = "Age_like",
             main = "Changes in age-composition likelihoods",
             profile.string = para,
             profile.label = label,
             ylab = "Change in -log-likelihood",
            legendloc = "topright",
            ymax = y.max2)
  
  PinerPlot (summaryoutput = profilesummary,
             plot = TRUE, print = FALSE,
             component = "Surv_like",
             main = "Changes in survey likelihoods",
             profile.string = para,
             profile.label = label,
             ylab = "Change in -log-likelihood",
            legendloc = "topright",
            ymax = y.max2)
dev.off()

}