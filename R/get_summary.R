#' Generate likelihood profiles
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#' 
#'
#' @template mydir
#' @param name Identify if the csv file show jitter, retro, or profile results
#' @param para SS parameter name that the profile was run across 
#' @param vec vector of parameter values the profile covers
#' @param profilemodels object created by the SSgetoutput funciton
#' @param profilesummary object created by the SSsummarize funciton
#' 
#' @author Chantel Wetzel & Kelli Johnson
#' @export

get_summary <- function(mydir, para, vec, name, profilemodels, profilesummary){

	# Need to identify a way to determine if a model estmates male growth parameters as offsets from females

	# get output
	outputs <- profilemodels
	quants  <- lapply(outputs, "[[", "derived_quants")
	status  <- sapply(sapply(outputs, "[[", "parameters", simplify = FALSE), "[[", "Status")
	bounds  <- apply(status, 2, function(x) rownames(outputs[[1]]$parameters)[x %in% c("LO", "HI")])

	out     <- data.frame("run" = gsub("replist", "", names(outputs)),
						  "profile_parameter" = para,
						  "parameter_value" = vec, 
	  					  "likelihood" = sapply(sapply(outputs, "[[", "likelihoods_used", simplify = FALSE), "[", 1, 1),
	  					  "gradient" = sapply(outputs, "[[", "maximum_gradient_component"),
	  					  "SB0" = sapply(quants, "[[", "SSB_Virgin", "Value"),
	  					  "SBfinal" = sapply(quants, "[[", paste0("SSB_", outputs[[1]]$endyr), "Value"),
	  					  "Deplfinal" = sapply(quants, "[[", paste0("Bratio_", outputs[[1]]$endyr), "Value"),
	  					  #"Fmsy" = sapply(quants, "[[", "annF_MSY", "Value"),
	  					  "Nparsonbounds" = apply(status, 2, function(x) sum(x %in% c("LO", "HI"))),
	  					  stringsAsFactors = FALSE)

	# write tables
	write.csv(x = table(unlist(bounds)), file= file.path(mydir, paste0(name, "_parsonbounds.csv")), row.names=FALSE)

	write.csv(x = out, file = file.path(mydir, paste0(name, "_results.csv")), row.names = FALSE)

	x <- profilesummary
	n <- x$n
	endyr <- x$endyrs[1]
	out <- data.frame(
					  totlikelihood = as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1:n]), 
        			  surveylike = as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1:n]), 
        			  discardlike = as.numeric(x$likelihoods[x$likelihoods$Label == "Discard",1:n]), 
        			  lengthlike = as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1:n]), 
        			  agelike = as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1:n]), 
        			  recrlike = as.numeric(x$likelihoods[x$likelihoods$Label == "Recruitment",1:n]), 
        			  forerecrlike = as.numeric(x$likelihoods[x$likelihoods$Label == "Forecast_Recruitment",1:n]),
        			  priorlike = as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_priors",1:n]), 
        			  parmlike = as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_devs",1:n]), 
        			  R0 = as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1:n]), 
        			  SB0 = as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1:n]),
        			  SBfinal = as.numeric(x$SpawnBio[x$SpawnBio$Label == paste0("SSB_", endyr), 1:n]),
        			  deplfinal = as.numeric(x$Bratio[x$Bratio$Label == paste0("Bratio_", endyr), 1:n]), 
        			  yieldspr = as.numeric(x$quants[x$quants$Label == "Dead_Catch_SPR", 1:n]),
        			  steep = as.numeric(x$pars[x$pars$Label == "SR_BH_steep", 1:n]),
        			  mfem = as.numeric(x$pars[x$pars$Label == "NatM_p_1_Fem_GP_1", 1:n]),
        			  lminfem = as.numeric(x$pars[x$pars$Label == "L_at_Amin_Fem_GP_1", 1:n]),
        			  lmaxfem = as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1:n]),
        			  kfem = as.numeric(x$pars[x$pars$Label == "VonBert_K_Fem_GP_1", 1:n]),
        			  cv1fem = as.numeric(x$pars[grep("young_Fem_GP_1", x$pars$Label), 1:n]),
        			  cv2fem = as.numeric(x$pars[grep("old_Fem_GP_1", x$pars$Label), 1:n]),
        			  mmale = as.numeric(x$pars[x$pars$Label =="NatM_p_1_Mal_GP_1", 1:n]),
        			  lminmale = as.numeric(x$pars[x$pars$Label =="L_at_Amin_Mal_GP_1", 1:n]),
        			  lmaxmale = as.numeric(x$pars[x$pars$Label =="L_at_Amax_Mal_GP_1", 1:n]),
        			  kmale = as.numeric(x$pars[x$pars$Label =="VonBert_K_Mal_GP_1", 1:n]),
        			  cv1male = as.numeric(x$pars[grep("young_Mal_GP_1", x$pars$Label), 1:n]),
        			  cv2male = as.numeric(x$pars[grep("old_Mal_GP_1", x$pars$Label), 1:n]), 
	  				  stringsAsFactors = FALSE)

	new_out = t(out)
	colnames(new_out) = vec
	if (para == "SR_LN(R0)") { colnames(new_out) =  paste0("R0 ", vec) }
	if (para == "NatM_p_1_Fem_GP_1") { colnames(new_out) =  paste0("M_f ", vec) }
	if (para == "NatM_p_1_Mal_GP_1") { colnames(new_out) =  paste0("M_m ", vec) }
	if (para == "SR_BH_steep") { colnames(new_out) =  paste0("h ", vec) }
	
	write.csv(x = new_out, file = file.path(mydir, paste0(name, "_quant_table.csv")), row.names = TRUE)

	return ()
}