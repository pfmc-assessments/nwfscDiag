#' Get Default Settings For Profiles
#' 
#' Create a matrix of default values for profiling over
#' the typical parameters given results will be presented to the
#' Pacific Fisheries Management Council.
#' 
#' The column titled 'param_space' indicated where the range of of the profile parameter
#' should be interpretted as relative to the base model estimate vs. across a pre-specified range.
#' An example is for R0 where the default setting below indicates that the param_space is relative
#' where the low bound for the profile is set = base model log(R0) - 2 and high = base model log(R0) + 2.
#' The default range for M is set in relative space to explore a range of (M - 0.40 * M) - (M + 0.40 * M) 
#' at a step size of 0.005. This range may be too large (or small) with a step size too large (or too small)
#' and should be considered if the default settings are appropriate for your specific model. One also could
#' opt to define the range in 'real' space which could be selected in the "param_space" column of the matrix.
#' 
#' @param parameters vector of SS parameter names to conduct a profile for
#' @param low a vector of low paramater bounds for the profile
#' @param high a vector of upper parameter bounds for the profile
#' @param step_size increments to run the profile between the low and high bounds
#' @param param_space options: real, mulitplier, relative indicates how to interpret the low and high bound values.
#' real indicates bounds in the parameter space, relative indicates how far to go from the base parameter, and
#' multiplier indicates that low and high bounds are set at x\% above and below the base parameter.
#'
#' @return A matrix of low, high, and step size values for the default parameters
#' that should be profiled. The goal is to provide users with a template
#' to add additional rows for parameters that they want to profile beyond
#' the default ones.
#'
#' @examples
#' \dontrun{
#' # Use the default settings
#' get_settings_profile()
#' }
#' 
#' @export
#'
get_settings_profile <- function( parameters =  c("NatM_p_1_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
								  low =  c(0.40, 0.25, -2),
								  high = c(0.40, 1.0,  2),
								  step_size = c(0.005, 0.05, 0.25),
								  param_space = c('multiplier', 'real', 'relative')) 
{
	
	if (length(parameters) != length(low) | 
	    length(parameters) != length(high) |
	    length(parameters) != length(step_size) |
	    length(parameters) != length(param_space) ){
	   	stop("Error: input vectors do match in length.")
	}

	out = data.frame( low = low, 
					  high = high, 
					  step_size = step_size, 
					  param_space = param_space)
	rownames(out) = parameters
  	return(out)
}




