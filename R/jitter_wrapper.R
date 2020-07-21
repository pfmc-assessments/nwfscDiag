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

	if(!file.exists(paste0(mydir, "/", model_settings$base_name, "/Report.sso"))) {
    	message("There is no Report.sso file in the base model directory", paste0(mydir, "/", model_settings$base_name))
    	stop()
  	}

	# Create a jitter folder with the same naming structure as the base model
	jitter_dir <- paste0(mydir, "/", model_settings$base_name, "_jitter_", model_settings$jitter_fraction)
  	dir.create(jitter_dir, showWarnings = FALSE)
  	all_files = list.files(paste0(mydir, "/", model_settings$base_name)) 
  	capture.output(file.copy(from = paste0(mydir, "/", model_settings$base_name,"/", all_files), 
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
                       init_values_src = model_settings$init_values_src
                       )	

  	#### Read in results using other r4ss functions
  	keys <- gsub("Report([0-9]+)\\.sso", "\\1", dir(jitter_dir, pattern = "^Report[0-9]"))
	keys <- type.convert(keys)[order(type.convert(keys))]
	profilemodels <- r4ss::SSgetoutput(dirvec = jitter_dir, keyvec = keys,
									   getcovar=FALSE, forecast=FALSE,
    								   verbose=FALSE, listlists=TRUE, underscore=FALSE,
    								   save.lists=FALSE
									   )
	# summarize output
	profilesummary <- r4ss::SSsummarize(profilemodels)
	save(profilesummary, file = paste0(jitter_dir, "/jitter_summary.Rdat"))

	like <- profilesummary$likelihoods[1, 1:length(keys)]
	ymax <- as.numeric(quantile(profilesummary$likelihoods[1, 1:length(keys)], 0.80))
	ymin <- 0 

	pngfun(wd = jitter_dir, file = paste0("Jiter_", model_settings$jitter_fraction, '.png'), h = 12, w = 9)
	plot(1:length(keys), like, ylim = c(ymin, ymax), ylab="NLL", xlab = "Iteration")
	abline(h = like[1], col = 'red', lwd = 2)
	abline(h =  min(like), col = 1, lty = 2)
	dev.off()

	# get output
	outputs <- profilemodels
	quants  <- lapply(outputs, "[[", "derived_quants")
	status  <- sapply(sapply(outputs, "[[", "parameters", simplify = FALSE), "[[", "Status")
	bounds  <- apply(status, 2, function(x) rownames(outputs[[1]]$parameters)[x %in% c("LO", "HI")])
	out     <- data.frame("run" = gsub("replist", "", names(outputs)),
	  					  "likelihood" = sapply(sapply(outputs, "[[", "likelihoods_used", simplify = FALSE), "[", 1, 1),
	  					  "gradient" = sapply(outputs, "[[", "maximum_gradient_component"),
	  					  "B0" = sapply(quants, "[[", "SSB_Virgin", "Value"),
	  					  "Fmsy" = sapply(quants, "[[", "annF_MSY", "Value"),
	  					  "Nparsonbounds" = apply(status, 2, function(x) sum(x %in% c("LO", "HI"))),
	  					  stringsAsFactors = FALSE)

	out[, "deltaNLL"] <- out[, "likelihood"] - out[row.names(out) == "replist0", "likelihood"]
	# write tables
	write.csv(x = table(unlist(bounds)), file = paste0(jitter_dir, "/jitter_parsonbounds.csv"), row.names = FALSE)
	write.csv(x = out, file = paste0(jitter_dir,"/jitter_results.csv"), row.names = FALSE)

	message("Finished jitters: you are an amazing!")
	
}