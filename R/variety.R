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
#' @note This measure is ill-defined for populations with only one class, so
#'   \code{standardized_simpson} will return 0 instead.
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

  if(length(n) == 1) {
    warning("standardized_simpson is ill-defined for populations with only one class. Returning 0.")
    return(0)
  }

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

#' Kvalseth's diversity
#'
#' \code{kvalseth} returns the Kvalseth diversity for a vector of class counts
#' in a popultation
#'
#' Kvalseth diversity is defined as \deqn{OD=(1-\sum \frac{n_i}{N}^2)^{-1}-1}
#' where \eqn{n_i} is the number of observations in the \eqn{i}th category and
#' \eqn{N} is the total number of all observations. Values are generally
#' negatively correlated with Simpson's diversity, with low diversity
#' approaching Inf, and high diversity approaching 0.
#'
#' @param n A numeric vector
#'
#' @references Kvålseth, Tarald O. “Note on Biological Diversity, Evenness, and
#'   Homogeneity Measures.” \emph{Oikos} 62, no. 1 (October 1991): 123–27.
#'   doi:\href{http://dx.doi.org/10.2307/3545460}{10.2307/3545460}.
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' kvalseth(p)

kvalseth <- function(n) {
  type_check(n)
  (1 - sum((n/sum(n))^2))^(-1) - 1
}

#' Kvalseth's diversity
#'
#' \code{kvalseth} returns the Kvalseth diversity for a vector of class counts
#' in a popultation
#'
#' Kvalseth diversity is defined as \deqn{OD=(1-\sum \frac{n_i}{N}^2)^{-1}-1}
#' where \eqn{n_i} is the number of observations in the \eqn{i}th category and
#' \eqn{N} is the total number of all observations. Values are generally
#' negatively correlated with Simpson's diversity, with low diversity
#' approaching Inf, and high diversity approaching 0.
#'
#' @param n A numeric vector
#'
#' @references Kvålseth, Tarald O. “Note on Biological Diversity, Evenness, and
#'   Homogeneity Measures.” \emph{Oikos} 62, no. 1 (October 1991): 123–27.
#'   doi:\href{http://dx.doi.org/10.2307/3545460}{10.2307/3545460}.
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' kvalseth(p)

kvalseth <- function(n) {
  type_check(n)
  (1 - sum((n/sum(n))^2))^(-1) - 1
}

#' Fager's diversity
#'
#' \code{fager} returns the Fager diversity for a vector of class counts in a
#' population.
#'
#' Fager's diversity is defined as \deqn{S=(\frac{k\sum \frac{n_i}{N}^{2} -
#' N^{2}}{k(k - 1)})^{\frac{1}{2}}}, where \eqn{n_i} is the number of
#' observations in the \eqn{i}th category, \eqn{N} is the total number of all
#' observations, and \eqn{k} is the number of categories.
#'
#' @param n A numeric vector
#'
#' @references Fager, E. W. “Diversity: A Sampling Study.” \emph{The American
#'   Naturalist} 106, no. 949 (May 1972): 293–310.
#'   \url{http://www.jstor.org/stable/2459778}
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' fager(p)

fager <- function(n) {
  type_check(n)
  ((length(n) * sum((n/sum(n))^2 - sum(n)^2)) / (length(n) * (length(n) - 1)))^(0.5)
}

#' Shannon's diversity
#'
#' \code{shannon} returns the Shannon diversity for a vector of class counts in
#' a population.
#'
#' Shannons's diversity is defined as \deqn{H = -\sum \frac{n_i}{N}^{2} * \ln
#' \frac{n_i}{N}^{2}}, where \eqn{n_i} is the number of observations in the
#' \eqn{i}th category and \eqn{N} is the total number of all observations.
#'
#' @param n A numeric vector
#'
#' @references Shannon, Claude Elwood, and Warren Weaver. \emph{The Mathematical
#'   Theory of Communication}. Urbana: University of Illinois Press, 1949.
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' shannon(p)

shannon <- function(n) {
  type_check(n)
  -sum((n/sum(n)) * log(n/sum(n)))
}

#' Hall and Tideman's diversity
#'
#' \code{hall_tideman} returns the Hall and Tideman's diversity for a vector of
#' class counts in a population.
#'
#' Hall and Tideman's diversity is defined as \deqn{TH = 1/(2\sum
#' {r_i}\frac{n_i}{N}^{2}) - 1}, where \eqn{n_i} is the number of observations
#' in the \eqn{i}th category, \eqn{N} is the total number of all observations,
#' and \eqn{r} is the rank of the \eqn{i}th category, with 1 being the largest
#' category
#'
#' @param n A numeric vector
#'
#' @references Hall, Marshall, and Nicolaus Tideman. “Measures of
#'   Concentration.” \emph{Journal of the American Statistical Association} 62,
#'   no. 317 (March 1967): 162–68.
#'   doi:\href{http://dx.doi.org/10.1080/01621459.1967.10482897}{10.1080/01621459.1967.10482897}.
#'
#'
#' @examples
#' p <- c(5, 10, 30, 1, 5)
#' hall_tideman(p)

hall_tideman <- function(n) {
  type_check(n)
  1 / (2 * sum(rank(n, ties = "random") * (n/sum(n))) - 1)
}
