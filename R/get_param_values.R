#' Generate likelihood profiles
#' To be called from the run_diagnostics function after creating
#' the model settings using the get_settings function.
#'
#'
#' @template mydir
#' @param para A character string specifying the SS3 parameter name that the
#'   profile pertains to. The parameter name should match the name in the
#'   control.ss_new file from SS3.
#' @param vec Vector of parameter values or retrospective runs the summary object contains
#' @param summary List created by the [r4ss::SSsummarize()] function
#'
#' @author Chantel Wetzel & Kelli Johnson
#' @export

get_param_values <- function(mydir, para = NULL, vec, summary) {
  x <- summary
  n <- x[["n"]]
  endyr <- x[["endyrs"]][1] + 1
  out <- data.frame(
    totlikelihood = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "TOTAL", 1:n]),
    surveylike = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "Survey", 1:n]),
    discardlike = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "Discard", 1:n]),
    lengthlike = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "Length_comp", 1:n]),
    agelike = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "Age_comp", 1:n]),
    recrlike = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "Recruitment", 1:n]),
    forerecrlike = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "Forecast_Recruitment", 1:n]),
    priorlike = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "Parm_priors", 1:n]),
    parmlike = as.numeric(x[["likelihoods"]][x[["likelihoods"]][["Label"]] == "Parm_devs", 1:n]),
    R0 = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "SR_LN(R0)", 1:n]),
    SB0 = as.numeric(x[["SpawnBio"]][x[["SpawnBio"]][["Label"]] == "SSB_Virgin", 1:n]),
    SBfinal = as.numeric(x[["SpawnBio"]][x[["SpawnBio"]][["Label"]] == paste0("SSB_", endyr), 1:n]),
    deplfinal = as.numeric(x[["Bratio"]][x[["Bratio"]][["Label"]] == paste0("Bratio_", endyr), 1:n]),
    yieldspr = as.numeric(x[["quants"]][x[["quants"]][["Label"]] == "Dead_Catch_SPR", 1:n]),
    steep = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "SR_BH_steep", 1:n]),
    mfem = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "NatM_uniform_Fem_GP_1", 1:n]),
    lminfem = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "L_at_Amin_Fem_GP_1", 1:n]),
    lmaxfem = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "L_at_Amax_Fem_GP_1", 1:n]),
    kfem = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "VonBert_K_Fem_GP_1", 1:n]),
    cv1fem = as.numeric(x[["pars"]][grep("young_Fem_GP_1", x[["pars"]][["Label"]]), 1:n]),
    cv2fem = as.numeric(x[["pars"]][grep("old_Fem_GP_1", x[["pars"]][["Label"]]), 1:n]),
    mmale = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "NatM_uniform_Mal_GP_1", 1:n]),
    lminmale = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "L_at_Amin_Mal_GP_1", 1:n]),
    lmaxmale = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "L_at_Amax_Mal_GP_1", 1:n]),
    kmale = as.numeric(x[["pars"]][x[["pars"]][["Label"]] == "VonBert_K_Mal_GP_1", 1:n]),
    cv1male = as.numeric(x[["pars"]][grep("young_Mal_GP_1", x[["pars"]][["Label"]]), 1:n]),
    cv2male = as.numeric(x[["pars"]][grep("old_Mal_GP_1", x[["pars"]][["Label"]]), 1:n]),
    stringsAsFactors = FALSE
  )

  out <- t(out)
  colnames(out) <- vec

  if (!is.null(para)) {
    name <- para
    if (para == "SR_LN(R0)") {
      colnames(out) <- paste0("R0 ", vec)
    }
    if (para == "NatM_uniform_Fem_GP_1") {
      colnames(out) <- paste0("M_f ", vec)
    }
    if (para == "NatM_uniform_Mal_GP_1") {
      colnames(out) <- paste0("M_m ", vec)
    }
    if (para == "SR_BH_steep") {
      colnames(out) <- paste0("h ", vec)
    }
  }

  utils::write.csv(x = out, file = file.path(mydir, paste0(name, "_quant_table.csv")), row.names = TRUE)
}
