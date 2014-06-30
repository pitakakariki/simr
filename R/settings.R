.simrOptions <- new.env(parent=emptyenv())

.simrOptions $ nSim <- 1000
.simrOptions $ light <- 150
.simrOptions $ progress <- TRUE
.simrOptions $ binom <- "exact"


#' Set default number of simulations
#'
#' Set the default number of simulations for each power analysis.
#'
#' @param nSim new value for the default number of simulations.
#'
#' @examples
#' setSimrNSim(10)
#'
#' @export
setSimrNSim <- function(nSim) assign("nSim", nSim, env=.simrOptions) #.simrOptions$nSim <- nSim



#' Set the default progress bar behaviour
#'
#' @param x \code{TRUE} or \code{FALSE} should simulations use a progress bar?
#'
#' @export
#'
setSimrProgress <- function(x) {
    
    if(x) .simrOptions$progress <- TRUE else .simrOptions$progress <- FALSE
}


