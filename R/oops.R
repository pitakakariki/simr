#' Recover an unsaved simulation
#'
#' Simulations can take a non-trivial time to run. If the user forgets to assign
#' the result to a variable this method can recover it.
#'
#' @export
#'
#' @examples
#' lm1 <- lmer(y ~ x + (1|g), data=example)
#' powerSim(lm1, nsim=25)
#' ps1 <- lastResult()
#'
#' @seealso \code{\link[base]{.Last.value}}
#'
lastResult <- function() {

  if(exists('.SIMRLASTRESULT')) return(.SIMRLASTRESULT)

  stop("No result available to recover.")
}
