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
#' @seealso \code{\link{standardized_simpson}}
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
#' @seealso \code{\link{simpson}}
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' standardized_simpson(p)

standardized_simpson <- function(n) {
  type_check(n)
  (1 - sum((n/sum(n))^2))/(1 - 1/length(n))
}

#' Junge's diversity.
#'
#' \code{junge} returns the Junge diversity for a vector of class counts in a
#' population
#'
#' Junge's diversity is defined as: \deqn{H=(1-\sqrt{k})(\sqrt{k-1}-\sqrt{k\sum
#' \frac{n_i}{N}^{2}}-1)} where \eqn{n_i} is the number of observations in the
#' \eqn{i}th category, \eqn{N} is the total number of all observations, and
#' \eqn{k} is the number of categories.
#'
#' @param n A numeric vector
#'
#' @references Junge, Kenneth. “Diversity of Ideas about Diversity Measurement.”
#'   \emph{Scandinavian Journal of Psychology} 35, no. 1 (1994): 16–26.
#'   doi:\href{http://dx.doi.org/10.1111/j.1467-9450.1994.tb00929.x}{10.1111/j.1467-9450.1994.tb00929.x}.
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' junge(p)

junge <- function(n) {
  type_check(n)
  (1 - sqrt(length(n))) * (sqrt(length(n) - 1) - sqrt(length(n) * sum((n/sum(n))^2)) - 1)
}
