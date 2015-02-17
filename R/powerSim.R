#' Estimate power by simulation.
#'
#' Performs a simulation analysis for a mixed model.
#'
#' @param fit a fitted model object (see \code{\link{doFit}}).
#' @param test specify the test to perform. By default, the first fixed effect in \code{fit} will be tested.
#'     (see: \link{tests}).
#' @param sim an object to simulate from. By default this is the same as \code{fit} (see \code{\link{doSim}}).
#' @param nsim the number of simulations to run.
#' @param alpha the significance level for the statistical test. Defaults to 0.05.
#' @param seed specify a random number generator seed, for reproducible results.
#' @param ... any additional arguments are passed on to \code{\link{doFit}}.
#'
#' @examples
#' fm1 <- lmer(y ~ x + (1|g), data=example)
#' powerSim(fm1, nSim=10)
#'
#' @export
powerSim <- function(

    fit,
    test = fixed(getDefaultXname(fit)),
    sim = fit,

    nsim = getSimrOption("nsim"),
    alpha = 0.05,

    seed,

    ...

    ) {

    # setup
    if(!missing(seed)) set.seed(seed)
    #this.frame <- getFrame(fit)

    test(fit) # throw any errors now

    # generate the simulations
    simulations <- maybe_rlply(nsim, doSim(sim), .text="Simulating")

    # fit the model to the simualtions
    z <- maybe_llply(simulations, doFit, fit, .text="Fitting", ...)

    # summarise the fitted models
    test <- wrapTest(test)
    p <- maybe_laply(z, test, .text="Testing")

    success <- sum(p$value < alpha, na.rm=TRUE)
    trials <- sum(!is.na(p$value))

    # structure the return value
    rval <- list()

    rval $ x <- success
    rval $ n <- trials

    #rval $ xname <- xname
    #rval $ effect <- fixef(sim)[xname] # can't guarantee this is available?

    rval $ text <- attr(test, "text")
    rval $ description <- attr(test, "description")

    rval $ pval <- p$value

    rval $ warnings <- p$warnings
    rval $ errors <- p$errors

    class(rval) <- "powerSim"

    .SIMRLASTRESULT <<- rval

    return(rval)
}

#' @export
print.powerSim <- function(x, ...) {

    cat(x$text)
    cat(", (95% confidence interval):\n")
    printerval(x, ...)
    cat("\n\n")

    pad <- "Test: "
    for(text in x$description) {
        cat(pad); pad <- "      "
        cat(text)
        cat("\n")
    }
    cat("\n")

    #cat(sprintf("Based on %i simulations and effect size %.2f", z$n, z$effect))
    cat(sprintf("Based on %i simulations, ", x$n))
    wn <- nrow(x$warnings) ; en <- nrow(x$errors)
    wstr <- str_c(wn, " ", if(wn==1) "warning" else "warnings")
    estr <- str_c(en, " ", if(en==1) "error" else "errors")
    cat(str_c("(", wstr, ", ", estr, ")"))
    cat("\n")
}

#' @export
plot.powerSim <- function(x, ...) stop("Not yet implemented.")
