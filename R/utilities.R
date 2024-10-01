#' Round values
#'
#' @param x Numeric value to round.
#' @param accuracy The number of digits to round to.
#' @param f R function to apply. Default is round().
#' @export
#'
round_any <- function(x, accuracy, f = round) {
  return(f(x / accuracy) * accuracy)
}
