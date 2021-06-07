#' Code to run jitters for a model
#' Output will be saved in an Rdata object called "jitter_output"
#' Plots and tables generated to visualize results
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

	# Read in the base model
  	base <- r4ss::SS_output(file.path(mydir, model_settings$base_name), covar = FALSE,
  							printstats = FALSE, verbose = FALSE)
	est  <- base$likelihoods_used[1, 1]
	like <- as.numeric(profilesummary$likelihoods[1, keys])
	ymax <- as.numeric(quantile(profilesummary$likelihoods[1, keys], 0.80))
	ymin <- min(like - est) + 1 

	jitter_output <- list()
	jitter_output$plotdir <-jitter_dir
	jitter_output$est  <- est
	jitter_output$keys <- keys
	jitter_output$like <- like
	jitter_output$model_settings <- model_settings
	jitter_output$profilesummary <- profilesummary 
	jitter_output$profilemodels  <- profilemodels
	save(jitter_output, file = file.path(jitter_dir, "jitter_output.Rdata"))

	pngfun(wd = jitter_dir, file = paste0("Jitter_", model_settings$jitter_fraction, '.png'), h = 12, w = 9)
	plot(keys, like-est, ylim = c(ymin, ymax), cex.axis = 1.25, cex.lab = 1.25,
		ylab="Change in Negative Log-likelihood", xlab = "Iteration")
	abline(h = 0, col = 'darkgrey', lwd = 2)
	find = which(est == like)
	points(keys[find], (like-est)[find], col = 'green3', pch = 16, cex = 1.1)
	find = which(like - est > 0)
	points(keys[find], (like-est)[find], col = 'blue', pch = 16)
	if (sum(like - est < 0) > 0) {
		find = like - est < 0
		points(keys[find], (like-est)[find], col = 'red', pch = 16, cex = 1.1)
		mtext(side = 3, cex = 1.25,
			"Warning: A lower NLL was found. Update your base model.")
	}
	legend('topleft', legend = c("Base Model Likelihood", "Higher Likelihood", "Lower Likelihood"),
		bty = 'n', pch = 16, col = c('green3', 'blue', 'red'))
	dev.off()

	if (ymax > 100){
		pngfun(wd = jitter_dir, file = paste0("Jitter_Zoomed_SubPlot_", model_settings$jitter_fraction, '.png'), h = 12, w = 9)
		plot(keys, like-est, ylim = c(ymin, 100), cex.axis = 1.25, cex.lab = 1.25,
			ylab="Change in Negative Log-likelihood", xlab = "Iteration")
		abline(h = 0, col = 'darkgrey', lwd = 2)
		find = which(est == like)
		points(keys[find], (like-est)[find], col = 'green3', pch = 16, cex = 1.1)
		find = which(like - est > 0)
		points(keys[find], (like-est)[find], col = 'blue', pch = 16)
		if (sum(like - est < 0) > 0) {
			find = like - est < 0
			points(keys[find], (like-est)[find], col = 'red', pch = 16, cex = 1.1)
			mtext(side = 3, cex = 1.25,
				"Warning: Only jitters near the base model shown")
		}
		legend('topleft', legend = c("Base Model Likelihood", "Higher Likelihood", "Lower Likelihood"),
			bty = 'n', pch = 16, col = c('green3', 'blue', 'red'))
		dev.off()

	}

	# get output
	outputs <- profilemodels
	quants  <- lapply(outputs, "[[", "derived_quants")
	status  <- sapply(sapply(outputs, "[[", "parameters", simplify = FALSE), "[[", "Status")
	bounds  <- apply(status, 2, function(x) rownames(outputs[[1]]$parameters)[x %in% c("LO", "HI")])
	out     <- data.frame("run" = gsub("replist", "", names(outputs)),
	  					  "likelihood" = sapply(sapply(outputs, "[[", "likelihoods_used", simplify = FALSE), "[", 1, 1),
	  					  "gradient" = sapply(outputs, "[[", "maximum_gradient_component"),
	  					  "SB0" = sapply(quants, "[[", "SSB_Virgin", "Value"),
	  					  "SBfinal" = sapply(quants, "[[", paste0("SSB_", profilesummary$endyrs[1]), "Value"),
	  					  "Nparsonbounds" = apply(status, 2, function(x) sum(x %in% c("LO", "HI"))),
	  					  "Lowest NLL" = ifelse(min(like) == like, "Best Fit", 0),
	  					  stringsAsFactors = FALSE)
	

	# write tables
	write.csv(x = table(unlist(bounds)), file = file.path(jitter_dir, "jitter_parsonbounds.csv"), row.names = FALSE)
	write.csv(x = out, file = file.path(jitter_dir, "jitter_results.csv"), row.names = FALSE)

	message("Finished jitters.")
	
}