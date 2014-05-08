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
    
    seed = NA,
    
    ...

    ) {

    # setup
    if(!is.na(seed)) set.seed(seed)
    this.frame <- getFrame(fit)
    
    # generate the simulations
    ##TODO## clean this up once plyr gets fixed
    #simulations <- replicate(R, doSim(sim), simplify=FALSE)
    simulations <- llply(1:nSim, function(.) doSim(sim), .progress=progress_simr("Simulating"))

    # fit the model to the simualtions
    z <- llply(simulations, doFit, fit, .progress=progress_simr("Fitting"), ...)
    
    # summarise the fitted models
    if(missing('test')) test <- getDefaultTest(fit, nSim=nSim, ...)
    p <- laply(z, test, .progress=progress_simr("Testing"))
    
    success <- sum(p < 0.05)
    
    # structure the return value
    rval <- structure(list(x=success, n=nSim, pval=p), class='poweranalysis')
    
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