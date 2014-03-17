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