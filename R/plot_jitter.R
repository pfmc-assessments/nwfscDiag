#' Generated plots to visualize results
#'
#'
#' @template mydir
#' @param output List of model output created by [run_jitter()].
#' @template model_settings
#'
#' @author Chantel Wetzel
#' @return
#' Nothing is explicitly returned from (plot_jitter[]).
#'
#'
#' @export

plot_jitter <- function(mydir, model_settings, output) {
  keys <- output[["keys"]]
  jitter_dir <- output[["plotdir"]]
  like <- output[["like"]]
  est <- output[["est"]]
  profilesummary <- output[["profilesummary"]]

  ymax <- as.numeric(stats::quantile(unlist(profilesummary[["likelihoods"]][1, keys]), 0.80))
  ymin <- min(like - est) + 1
  ylab <- "Change in negative log-likelihood"
  xlab <- "Iteration"
  pngfun(wd = jitter_dir, file = "jitter.png", h = 12, w = 9)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(keys, like - est,
    ylim = c(ymin, ymax), cex.axis = 1.25, cex.lab = 1.25,
    ylab = ylab, xlab = xlab
  )
  graphics::abline(h = 0, col = "darkgrey", lwd = 2)
  find <- which(est == like)
  graphics::points(keys[find], (like - est)[find], col = "green3", pch = 16, cex = 1.1)
  find <- which(like - est > 0)
  graphics::points(keys[find], (like - est)[find], col = "blue", pch = 16)
  if (sum(like - est < 0) > 0) {
    find <- like - est < 0
    graphics::points(keys[find], (like - est)[find], col = "red", pch = 16, cex = 1.1)
    graphics::mtext(
      side = 3, cex = 1.25,
      "Warning: A lower NLL was found. Update your base model."
    )
  }
  graphics::legend("topleft",
    legend = c("Base Model Likelihood", "Higher Likelihood", "Lower Likelihood"),
    bty = "n", pch = 16, col = c("green3", "blue", "red")
  )
  dev.off()

  if (ymax > 100) {
    pngfun(wd = jitter_dir, file = "jitter_zoomed.png", h = 12, w = 9)
    on.exit(grDevices::dev.off(), add = TRUE)
    plot(keys, like - est,
      ylim = c(ymin, 100), cex.axis = 1.25, cex.lab = 1.25,
      ylab = ylab, xlab = xlab
    )
    graphics::abline(h = 0, col = "darkgrey", lwd = 2)
    find <- which(est == like)
    graphics::points(keys[find], (like - est)[find], col = "green3", pch = 16, cex = 1.1)
    find <- which(like - est > 0)
    graphics::points(keys[find], (like - est)[find], col = "blue", pch = 16)
    if (sum(like - est < 0) > 0) {
      find <- like - est < 0
      graphics::points(keys[find], (like - est)[find], col = "red", pch = 16, cex = 1.1)
      graphics::mtext(
        side = 3, cex = 1.25,
        "Warning: Only jitters near the base model shown"
      )
    }
    graphics::legend("topleft",
      legend = c("Base Model Likelihood", "Higher Likelihood", "Lower Likelihood"),
      bty = "n", pch = 16, col = c("green3", "blue", "red")
    )
  }
}
