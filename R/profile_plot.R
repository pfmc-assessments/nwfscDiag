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

profile_plot <- function(mydir, model_settings, para_name, profilesummary){

  label = ifelse(para_name == "SR_LN(R0)", expression(log(italic(R)[0])),
  	      ifelse(para_name == "NatM_p_1_Fem_GP_1", "Natural Mortality (female)",
  	      ifelse(para_name == "NatM_p_1_Mal_GP_1", "Natural Mortality (male)",
  	      ifelse(para_name == "steep", "Steepness (h)",
  	      para_name))))

  get = ifelse(para_name == "SR_LN(R0)", "R0",
  	      ifelse(para_name == "NatM_p_1_Fem_GP_1", "NatM_p_1_Fem_GP_1",
  	      ifelse(para_name == "NatM_p_1_Mal_GP_1", "NatM_p_1_Mal_GP_1",
  	      ifelse(para_name == "steep", "steep",
  	      para_name))))

  y.max = max(profilesummary$likelihoods[1,1:length(profilemodels)]) - min(profilesummary$likelihoods[1,1:length(profilemodels)])
  len.y = max(profilesummary$likelihoods[8,1:length(profilemodels)]) - min(profilesummary$likelihoods[8,1:length(profilemodels)])
  age.y = max(profilesummary$likelihoods[9,1:length(profilemodels)]) - min(profilesummary$likelihoods[9,1:length(profilemodels)])
  survey.y = max(profilesummary$likelihoods[4,1:length(profilemodels)]) - min(profilesummary$likelihoods[4,1:length(profilemodels)])
  y.max2 = max(len.y, age.y, survey.y)

  png(file.path(mydir, "piner_panel_", para_name, ".png"), height = 7, width = 7, units = "in", res = 300)
  par(mfrow = c(2,2))
  SSplotProfile(summaryoutput = profilesummary, 
  			    main = "Changes in total likelihood", 
  			    profile.string = get, 
                profile.label = label,
                ymax = y.max)
  
  PinerPlot (summaryoutput = profilesummary,
             plot=TRUE, print=FALSE,
             component = "Length_like",
             main = "Length-composition likelihoods",
             profile.string = get,
             profile.label = label,
             ylab = "Change in -log-likelihood",
             legendloc = "topright", 
             ymax = y.max2)
  
  PinerPlot (summaryoutput = profilesummary,
             plot = TRUE, print = FALSE,
             component = "Age_like",
             main = "Age-composition likelihoods",
             profile.string = get,
             profile.label = label,
             ylab = "Change in -log-likelihood",
            legendloc = "topright",
            ymax = y.max2)
  
  PinerPlot (summaryoutput = profilesummary,
             plot = TRUE, print = FALSE,
             component = "Surv_like",
             main = "Survey likelihoods",
             profile.string = get,
             profile.label = label,
             ylab = "Change in -log-likelihood",
            legendloc = "topright",
            ymax = y.max2)
dev.off()

}