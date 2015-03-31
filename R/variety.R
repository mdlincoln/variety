#' Simpson's diversity.
#'
#' \code{simpson} returns the Simpson diversity for a vector of class counts in
#' a population
#'
#' Simpson's diversity is defined as: \deqn{D = 1 - \sum \frac{n_i}{N}^{2}}
#' where \eqn{n_i} is the number of observations in the \eqn{i}th category, and
#' \eqn{N} is the total number of all observations.
#'
#' @param n A numeric vector
#'
#' @references Simpson, E. H. “Measurement of Diversity.” \emph{Nature} 163, no.
#'   4148 (April 30, 1949): 688–688.
#'   doi:\href{http://dx.doi.org/10.1038/163688a0}{10.1038/163688a0}.
#'
#' @seealso \link{standardized_simpson}
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' simpson(p)

simpson <- function(n) {
  type_check(n)
  1 - sum((n/sum(n))^2)
}

#' Standardized Simpson's diversity.
#'
#' \code{standardized_simpson} returns the standardized Simpson diversity for a
#' vector of class counts in a population
#'
#' Standardized Simpson's diversity is defined as: \deqn{D_z = \frac{1 - \sum
#' \frac{n_i}{N}^{2}}{1 - \frac{1}{k}}} where \eqn{n_i} is the number of
#' observations in the \eqn{i}th category, \eqn{N} is the total number of all
#' observations, and \eqn{k} is the number of categories. This allows comparison
#' of diversity values across distributions with different numbers of
#' categories.
#'
#' @param n A numeric vector
#'
#' @references Simpson, E. H. “Measurement of Diversity.” \emph{Nature} 163, no.
#'   4148 (April 30, 1949): 688–688.
#'   doi:\href{http://dx.doi.org/10.1038/163688a0}{10.1038/163688a0}.
#'
#' @seealso \link{simpson}
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' standardized_simpson(p)

standardized_simpson <- function(n) {
  type_check(n)
  (1 - sum((n/sum(n))^2))/(1 - 1/length(n))
}
