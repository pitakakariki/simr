#' Estimate power by simulation.
#'
#' Perform a power analysis for a mixed model.
#'
#' @param fit a fitted model object (see \code{\link{doFit}}).
#' @param test specify the test to perform. By default, the first fixed effect in \code{fit} will be tested.
#'     (see: \link{tests}).
#' @param sim an object to simulate from. By default this is the same as \code{fit} (see \code{\link{doSim}}).
#' @param seed specify a random number generator seed, for reproducible results.
#' @param fitOpts extra arguments for \code{\link{doFit}}.
#' @param testOpts extra arguments for \code{\link{doTest}}.
#' @param simOpts extra arguments for \code{\link{doSim}}.
#' @param ... any additional arguments are passed on to \code{\link{simrOptions}}. Common options include:
#' \describe{
#'   \item{\code{nsim}:}{the number of simulations to run (default is \code{1000}).}
#'   \item{\code{alpha}:}{the significance level for the statistical test (default is \code{0.05}).}
#'   \item{\code{progress}:}{use progress bars during calculations (default is \code{TRUE}).}
#'   }#'
#' @examples
#' fm1 <- lmer(y ~ x + (1|g), data=example)
#' powerSim(fm1, nSim=10)
#'
#' @export
powerSim <- function(

    fit,
    test = fixed(getDefaultXname(fit)),
    sim = fit,

    fitOpts = list(),
    testOpts = list(),
    simOpts = list(),

    seed,

    ...

    ) {

    opts <- simrOptions(...)

    nsim <- getSimrOption("nsim")

    # START TIMING
    start <- proc.time()

    # setup
    if(!missing(seed)) set.seed(seed)
    #this.frame <- getFrame(fit)

    #test(fit) # throw any errors now

    # generate the simulations
    #simulations <- maybe_rlply(nsim, doSim(sim), .text="Simulating")

    # fit the model to the simulations
    #z <- maybe_llply(simulations, doFit, fit, .text="Fitting", ...)

    # summarise the fitted models
    test <- wrapTest(test)
    #p <- maybe_laply(z, test, .text="Testing")

    f <- function() {

        # y <- doSim(sim, [opts])
        tag(y <- do.call(doSim, c(list(sim), simOpts)), tag="Simulating")

        # fit <- doFit(y, fit, [opts])
        tag(z <- do.call(doFit, c(list(y, fit), fitOpts)), tag="Fitting")

        # doTest(fit, test, [opts])
        tag({
            pval <- do.call(doTest, c(list(z, test), testOpts))
            if(!is.numeric(pval) || length(pval)!= 1) stop("Test did not return a p-value")
        }, tag="Testing")

        return(pval)
    }

    p <- maybe_raply(nsim, f(), .text="Simulating")

    success <- sum(p$value < getSimrOption("alpha"), na.rm=TRUE)
    trials <- sum(!is.na(p$value))

    # END TIMING
    timing <- proc.time() - start

    simrOptions(opts)

    # structure the return value
    rval <- list()

    rval $ x <- success
    rval $ n <- nsim

    #rval $ xname <- xname
    #rval $ effect <- fixef(sim)[xname] # can't guarantee this is available?

    rval $ text <- attr(test, "text")(fit)
    rval $ description <- attr(test, "description")(fit)

    rval $ pval <- p$value

    rval $ warnings <- p$warnings
    rval $ errors <- p$errors

    rval $ timing <- timing

    class(rval) <- "powerSim"

    .simrLastResult $ lastResult <- rval

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
    wn <- length(unique(x$warnings$index)) ; en <- length(unique(x$errors$index))
    wstr <- str_c(wn, " ", if(wn==1) "warning" else "warnings")
    estr <- str_c(en, " ", if(en==1) "error" else "errors")
    cat(str_c("(", wstr, ", ", estr, ")"))
    cat("\n")

    time <- x$timing['elapsed']
    cat(sprintf("\nTime elapsed: %i h %i m %i s\n", floor(time/60/60), floor(time/60) %% 60, floor(time) %% 60))
}

#' @export
plot.powerSim <- function(x, ...) stop("Not yet implemented.")
