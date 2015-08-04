#' Generate simulated response variables.
#'
#' This is normally an internal function, but it can be overloaded to extend \code{simr} to other packages.
#'
#' @param object an object to apply a statistcal test to, usually a fitted model.
#' @param a test function, see \link{tests}.
#'
#' @return a vector containing simulated response values (or, for models  with a multivariate response such as
#'     binomial gl(m)m's, a matrix of simulated response values). Suitable as input for \code{\link{doFit}}.
#'
#' @export
doTest <- function(object, test, ...) UseMethod('doTest', object)

#' @export
doTest.default <- function(object, test, ...) {

    wrapTest(test)(object, ...)
}