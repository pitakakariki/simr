#' Calculate the power for an analysis.
#'
#' @export
#'
#'
#'
#'
#'
#'
#'
#'
#'

powerSim <- function(
    
    fit,
    nSim = .SIMRDEFAULT_NSIM,
    sim = fit,
    
    xname = getDefaultXname(fit),

    test,
    alpha = 0.05,
    
    seed,
    
    ...

    ) {

    # setup
    if(!missing(seed)) set.seed(seed)
    this.frame <- getFrame(fit)
    
    # generate the simulations
    simulations <- maybe_rlply(nSim, doSim(sim), .text="Simulating")
    
    # fit the model to the simualtions
    z <- maybe_llply(simulations, doFit, fit, .text="Fitting", ...)
    
    # summarise the fitted models
    if(missing('test')) test <- getDefaultTest(fit, xname, nSim=nSim, ...)
    p <- maybe_laply(z, test, .text="Testing")
    
    success <- sum(p$value < alpha)
    
    # structure the return value
    rval <- structure(list(x=success, n=nSim, pval=p$value, warnings=p$warnings, errors=p$errors, test=test), class='poweranalysis')
    
    .SIMRLASTRESULT <<- rval
    
    return(rval)
}

#' @export
print.poweranalysis <- function(z) {
    
    interval <- 100 * binom.confint(z$x, z$n, 0.95, "logit")[c("mean", "lower", "upper")]   
    
    with(interval, print(sprintf("%.2f%% (%.2f, %.2f)", mean, lower, upper)))
}

#' @export
plot.poweranalysis <- function(...) stop("Not yet implemented.")