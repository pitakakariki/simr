#' Generate simulated response variables.
#'
#' This is normally an internal function, but it can be overloaded to extend \code{simr} to other packages.
#'
#' @param object an object to simulate from, usually a fitted model.
#' @param ... additional options.
#'
#' @return a vector containing simulated response values (or, for models  with a multivariate response such as
#'     binomial gl(m)m's, a matrix of simulated response values). Suitable as input for \code{\link{doFit}}.
#'
#' @export
doSim <- function(object, ...) UseMethod("doSim", object)

#' @export
doSim.default <- function(object, ...) {

    simulate(object, ...)[[1]]
}

#' @export
doSim.iter <- function(object, ...) {

    nextElem(object, ...)
}

#' @export
doSim.merMod <- function(object, ...) {

    simParams <- list(

            beta = fixef(object),
            theta = getME(object, "theta"),
            sigma = sigma(object)
    )

    useSc <- object@devcomp$dims["useSc"]
    if(!useSc) simParams$sigma <- NULL

    simData <- getData(object)

    # check weights
    w <- weights(object)
    if(!is.null(w)) if(length(w) != nrow(simData)) {

        if(length(unique(w)) != 1) {

            if(cbindResponse(object)) {

                w <- NULL

            } else {

                stop("Non-uniform weights are not supported yet.")
            }

        } else {

            w <- rep(w[1], nrow(simData))
        }
    }

    simulate(
        formula(object),
        newparams=simParams,
        newdata=simData,
        family=family(object),
        weights=w, ...)[[1]]
}

#' @export
doSim.function <- function(object, ...) object(...)





