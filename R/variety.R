#' Simpson's diversity.
#'
#' \code{simpson} returns the Simpson diversity for a vector of class counts in
#' a population
#'
#' Simpson's diversity is defined as: \deqn{D = 1 - \sum \frac{n_i}{N}^{2}}
#' where \eqn{n_i} is the number of observations within a given class, and
#' \eqn{N} is the total number of all observations.
#'
#' @param n A numeric vector
#'
#' @references Simpson, E. H. “Measurement of Diversity.” \emph{Nature} 163, no.
#'   4148 (April 30, 1949): 688–688.
#'   doi:\href{http://dx.doi.org/10.1038/163688a0}{10.1038/163688a0}.
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' simpson(p)

simpson <- function(n) {
  1 - sum((n/sum(n))^2)
}
