#' Recover an unsaved simulation
#'
#' Simulations can take a non-trivial time to run. If the user forgets to assign
#' the result to a variable this method can recover it.
#'
#' @export
#'
#'
#' @seealso \code{\link{Last.value}}
#'
lastResult <- function() {

  if(exists('.SIMRLASTRESULT')) return(.SIMRLASTRESULT)

  stop("No result available to recover.")
}
