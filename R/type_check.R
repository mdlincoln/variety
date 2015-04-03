#' Utility function for checking type.
#'
#' \code{type_check} throws an error if \code{x} is not a numeric vector.
#'
#' @param x An R object
#'
#' @keywords internal

type_check <- function(x) {
  if(!(class(x) %in% c("numeric", "integer")))
    stop("n must be of class \"numeric\" or \"integer\"")
}
