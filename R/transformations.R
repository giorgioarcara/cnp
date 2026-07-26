# Common predictor transformations used by the regression methods and
# simulation functions (age/education transformations tested by
# adjscores_A2024_*/adjscores_C1987_*, and sampled by sample.transf()).

#' Cube transformation
#'
#' @param x Numeric vector.
#' @return `x^3`.
#' @export
cube = function(x){x^3}

#' Quadratic transformation
#'
#' @param x Numeric vector.
#' @return `x^2`.
#' @export
quadr = function(x){x^2}

#' Log of (100 - x) transformation
#'
#' @param x Numeric vector.
#' @return `log(100 - x)`.
#' @export
logm100 = function(x){log(100-x)}

#' Log10 of (100 - x) transformation
#'
#' @param x Numeric vector.
#' @return `log10(100 - x)`.
#' @export
log10m100 = function(x){log10(100-x)}

#' Log10 of (mean(x) - x) transformation
#'
#' @param x Numeric vector.
#' @return `log10(mean(x) - x)`.
#' @export
log10mAve = function(x){log10(mean(x)-x)}

#' Inverse transformation
#'
#' @param x Numeric vector.
#' @return `1 / x`.
#' @export
inv = function(x){1/x}

#' Second-order orthogonal polynomial transformation
#'
#' @param x Numeric vector.
#' @return A matrix with the linear and quadratic orthogonal polynomial terms
#'   (see [stats::poly()]).
#' @export
poly2 = function(x){poly(x,2)}

#' Zero transformation (models no effect)
#'
#' @param x Numeric vector.
#' @return A vector of zeros, same length as `x`.
#' @export
zero = function(x){0*x} # to model no effect.
