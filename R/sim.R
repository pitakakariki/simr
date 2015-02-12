#' Generate simulated response variables.
#'
#'
#' @export
doSim <- function(simObj) UseMethod('doSim', simObj)

doSim.default <- function(simObj) {

    # if it's lm or lmer, use simulate.

    # option to use an iterator?

    # default for unknown objects?


    simulate(simObj)[[1]]
}

doSim.iter <- function(simObj) {

    nextElem(simObj)
}

doSim.merMod <- function(simObj) {

    simParams <- list(

            beta = fixef(simObj),
            theta = getME(simObj, "theta"),
            sigma = sigma(simObj)
    )

    useSc <- simObj@devcomp$dims["useSc"]
    if(!useSc) simParams$sigma <- NULL

    simData <- getData(simObj)

    simulate(formula(simObj), newparams=simParams, newdata=simData, family=family(simObj))[[1]]
}
