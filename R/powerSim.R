#' Calculate the power for an analysis.
#'
#' Performs a simulation analysis for a linear mixed model.
#'
#' @param fit fit
#' @param nSim nsim
#' @param sim sim
#' @param xname
#' @param test
#' @param alpha
#' @param seed
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
    nSim = .simrOptions$nSim,
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
    rval <- list()
    
    rval $ x <- success
    rval $ n <- nSim
  
    rval $ xname <- xname
    rval $ effect <- fixef(fit)[xname]

    rval $ pval <- p$value

    rval $ warnings <- p$warnings
    rval $ errors <- p$errors
    
    class(rval) <- "poweranalysis"

    .SIMRLASTRESULT <<- rval
    
    return(rval)
}

#' @export
print.poweranalysis <- function(z, ...) {

    cat("\rPower to detect effect of ")
    cat(z$xname)
    cat(", (95% confidence interval):\n")
    printerval(z, ...)
    cat("\n\n")

    cat(sprintf("Based on %i simulations and effect size %.2f", z$n, z$effect))
    cat("\n")  
}

printerval <- function(z, method="logit", ...) {
  
    interval <- 100 * binom.confint(z$x, z$n, 0.95, method)[c("mean", "lower", "upper")]   
    with(interval, cat(sprintf("%6.2f%% (%6.2f, %6.2f)", mean, lower, upper)))
}

#' @export
plot.poweranalysis <- function(...) stop("Not yet implemented.")