# Common predictor transformations used by the regression methods and
# simulation functions (age/education transformations tested by
# adjscores_A2024_*/adjscores_C1987_*, and sampled by sample.transf()).

#' Common predictor transformations
#'
#' Simple transformation functions applied to age/education predictors when
#' selecting the best-fitting model (see [adjscores_A2024_v1()],
#' [adjscores_C1987_v1()]) or when simulating data with a known generative
#' relationship (see [sim.norm.data()], [sample.transf()]).
#'
#' @param x Numeric vector.
#' @return A numeric vector (or, for `poly2`, a matrix): the transformed
#'   values of `x`.
#' @name transformations
NULL

#' @rdname transformations
#' @export
cube = function(x){x^3}

#' @rdname transformations
#' @export
quadr = function(x){x^2}

#' @rdname transformations
#' @export
logm100 = function(x){log(100-x)}

#' @rdname transformations
#' @export
log10m100 = function(x){log10(100-x)}

#' @rdname transformations
#' @export
log10mAve = function(x){log10(mean(x)-x)}

#' @rdname transformations
#' @export
inv = function(x){1/x}

#' @rdname transformations
#' @export
poly2 = function(x){poly(x,2)}

#' @rdname transformations
#' @export
zero = function(x){0*x} # to model no effect.
