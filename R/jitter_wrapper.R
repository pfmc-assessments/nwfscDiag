#' 
#'
#' 
#' 
#' 
#'
#' @param mydir Directory where model files are located.
#' @param model_settings input of all settings created using the get_settings function
#' 
#' @author Chantel Wetzel
#' @return A vector of likelihoods for each jitter iteration.
#' @export

jitter_wrapper <- function(mydir,  model_settings){

	if(!file.exists(file.path(mydir, model_settings$base_name, "Report.sso"))) {
    	message("There is no Report.sso file in the base model directory", file.path(mydir, model_settings$base_name))
    	stop()
  	}

	# Create a jitter folder with the same naming structure as the base model
	jitter_dir <- file.path(mydir,  paste0(model_settings$base_name, "_jitter_", model_settings$jitter_fraction))
  	dir.create(jitter_dir, showWarnings = FALSE)
  	all_files = list.files(file.path(mydir, model_settings$base_name)) 
  	capture.output(file.copy(from = file.path(mydir, model_settings$base_name, all_files), 
  			  to = jitter_dir, 
  			  overwrite = TRUE), file = "run_diag_warning.txt")
  	message("Running jitters: temporarily changing working directory to: ", jitter_dir)

  	r4ss::SS_RunJitter(mydir = jitter_dir,
                       model = model_settings$model,
                       extras = model_settings$extras,
                       Njitter = model_settings$Njitter,
                       Intern = model_settings$Intern,
                       systemcmd = model_settings$systemcmd,
                       printlikes = model_settings$printlikes,
                       verbose = model_settings$verbose,
                       jitter_fraction = model_settings$jitter_fraction,
                       init_values_src = model_settings$jitter_init_values_src )	

  	#### Read in results using other r4ss functions
  	#keys <- gsub("Report([0-9]+)\\.sso", "\\1", dir(jitter_dir, pattern = "^Report[0-9]"))
	#keys <- type.convert(keys)[order(type.convert(keys))]
	keys <- 1:model_settings$Njitter
	profilemodels <- r4ss::SSgetoutput(dirvec = jitter_dir, keyvec = keys,
									   getcovar = FALSE, forecast = FALSE,
    								   verbose = FALSE, listlists = TRUE, underscore = FALSE,
    								   save.lists = FALSE)

	# summarize output
	profilesummary <- r4ss::SSsummarize(profilemodels)
	save(profilesummary, file = file.path(jitter_dir, "jitter_summary.Rdat"))

	# Read in the base model
  	base <- r4ss::SS_output(file.path(mydir, model_settings$base_name), covar = FALSE,
  							printstats = FALSE, verbose = FALSE)
	est  <- base$likelihoods_used[1, 1]
	like <- profilesummary$likelihoods[1, keys]
	ymax <- as.numeric(quantile(profilesummary$likelihoods[1, keys], 0.80))
	ymin <- 0 

	#pngfun(wd = jitter_dir, file = paste0("Jitter_", model_settings$jitter_fraction, '.png'), h = 12, w = 9)
	#plot(keys, like, ylim = c(ymin, ymax), ylab="-log-likelihood", xlab = "Iteration")
	#abline(h = est, col = 'red', lwd = 2)
	#abline(h =  min(like), col = 1, lty = 2)
	#dev.off()

	# get output
	#outputs <- profilemodels
	#quants  <- lapply(outputs, "[[", "derived_quants")
	#status  <- sapply(sapply(outputs, "[[", "parameters", simplify = FALSE), "[[", "Status")
	#bounds  <- apply(status, 2, function(x) rownames(outputs[[1]]$parameters)[x %in% c("LO", "HI")])
	#out     <- data.frame("run" = gsub("replist", "", names(outputs)),
	#  					  "likelihood" = sapply(sapply(outputs, "[[", "likelihoods_used", simplify = FALSE), "[", 1, 1),
	#  					  "gradient" = sapply(outputs, "[[", "maximum_gradient_component"),
	#  					  "SB0" = sapply(quants, "[[", "SSB_Virgin", "Value"),
	#  					  "SBfinal" = sapply(quants, "[[", paste0("SSB_", profilesummary$endyrs[1]), "Value"),
	#  					  #"Fmsy" = sapply(quants, "[[", "annF_MSY", "Value"),
	#  					  "Nparsonbounds" = apply(status, 2, function(x) sum(x %in% c("LO", "HI"))),
	#  					  stringsAsFactors = FALSE)
#
	#out[, "deltaNLL"] <- out[, "likelihood"] - out[row.names(out) == "replist0", "likelihood"]
	## write tables
	#write.csv(x = table(unlist(bounds)), file = file.path(jitter_dir, "jitter_parsonbounds.csv"), row.names = FALSE)
	#write.csv(x = out, file = file.path(jitter_dir, "jitter_results.csv"), row.names = FALSE)

	message("Finished jitters.")
	
}