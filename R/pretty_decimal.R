#' Format a vector of real numbers with a pretty number of decimal places
#'
#' Choose the final formatting for the number of decimals that should be
#' displayed given a vector of real numbers. All numbers in the returned
#' vector will be unique.
#'
#' @param x A vector of real numbers.
#'
#' @author Kelli F. Johnson
#' @export
#' @return A vector of strings containing formatted numerical values all with
#' an equal number of decimal places and at least one decimal place.
#'
#' @examples
#' pretty_decimal(c(1.0, pi, 3.14, 0.002, 0.0021, 0.02))
#'
#' # Returns at least one decimal place even if not needed
#' pretty_decimal(1)
#'
#' # Will return an error because duplicated entries
#' \dontrun{
#' pretty_decimal(c(1.1, 1.1))
#' }
#'
pretty_decimal <- function(x) {
  # Check input
  stopifnot(is.numeric(x))
  stopifnot(!any(duplicated(x)))

  # Initialize xx with 1 decimal place
  xx <- sprintf(fmt = "%.1f", x)

  # Increment number of decimals until no entries are duplicated
  while (any(duplicated(xx))) {
    xx <- sprintf(
      fmt = paste0(
        "%.",
        nchar(gsub("[0-9]+\\.", "", xx)) + 1,
        "f"
      ),
      x
    )
  }

  # Return the formatted vector
  return(xx)
}
