#' Create a `.png` figure

#' @param wd A string providing the directory location where you want the
#'   figure saved.
#' @param file A string providing the name of the .png file that will be
#'   saved in `wd`.
#' @param w,h Numeric values providing the figure width and height in inches.
#' @param pt Numeric value providing the point size.
#' @author by Chantel Wetzel
#' @export
pngfun <- function(wd, file, w = 7, h = 7, pt = 12) {
  file <- file.path(wd, file)
  cat("writing PNG to", file, "\n")
  on.exit(grDevices::dev.off(), add = TRUE)
  grDevices::png(
    filename = file,
    width = w,
    height = h,
    units = "in",
    res = 300,
    pointsize = pt
  )
}
