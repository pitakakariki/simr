#' Recover an unsaved simulation
#'
#' Simulations can take a non-trivial time to run. If the user forgets to assign
#' the result to a variable this method can recover it.
#'
#' @export
#'
#' @examples
#' fm1 <- lmer(y ~ x + (1|g), data=simdata)
#' powerSim(fm1, nsim=10)
#' ps1 <- lastResult()
#'
#' @seealso \code{\link[base]{.Last.value}}
#'
lastResult <- function() {

  if(exists("lastResult", envir=.simrLastResult)) return(get("lastResult", envir=.simrLastResult))

  stop("No result available to recover.")
}

.simrLastResult <- new.env(parent=emptyenv())

