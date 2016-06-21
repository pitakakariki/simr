#' Estimate power at a range of sample sizes.
#'
#' This function runs \code{\link{powerSim}} over a range of sample sizes.
#'
#' @param fit a fitted model object (see \code{\link{doFit}}).
#' @param test specify the test to perform. By default, the first fixed effect in \code{fit} will be tested.
#'     (see: \link{tests}).
#' @param sim an object to simulate from. By default this is the same as \code{fit} (see \code{\link{doSim}}).
#' @param along the name of an explanatory variable. This variable will have its number of levels varied.
#' @param within names of grouping variables, separated by "+" or ",". Each combination of groups will be
#'               extended to \code{n} rows.
#' @param breaks number of levels of the variable specified by \code{along} at each point on the power curve.
#' @param seed specify a random number generator seed, for reproducible results.
#' @param fitOpts extra arguments for \code{\link{doFit}}.
#' @param testOpts extra arguments for \code{\link{doTest}}.
#' @param simOpts extra arguments for \code{\link{doSim}}.
#' @param ... any additional arguments are passed on to \code{\link{simrOptions}}. Common options include:
#' \describe{
#'   \item{\code{nsim}:}{the number of simulations to run (default is \code{1000}).}
#'   \item{\code{alpha}:}{the significance level for the statistical test (default is \code{0.05}).}
#'   \item{\code{progress}:}{use progress bars during calculations (default is \code{TRUE}).}
#'   }
#'
#' @examples
#' \dontrun{
#' fm <- lmer(y ~ x + (1|g), data=simdata)
#' pc1 <- powerCurve(fm)
#' pc2 <- powerCurve(fm, breaks=c(4,6,8,10))
#' print(pc2)
#' plot(pc2)
#' }
#'
#' @seealso \code{\link{print.powerCurve}}, \code{\link{summary.powerCurve}}, \code{\link{confint.powerCurve}}
#'
#' @export
powerCurve <- function(

    fit,
    test = fixed(getDefaultXname(fit)),
    sim = fit,

    along = getDefaultXname(fit),
    within,
    breaks,

    seed,

    fitOpts = list(),
    testOpts = list(),
    simOpts = list(),

    ...

    ) {

    opts <- simrOptions(...)
    on.exit(simrOptions(opts))

    # START TIMING
    start <- proc.time()

    nsim <- getSimrOption("nsim")
    if(!missing(seed)) set.seed(seed)

    # auto subsetting

    data <- getData(sim)

    if(!missing(along) && !missing(within)) stop("Only one of along and within may be used.")

    if(!missing(within)) {

        data <- addReplicateIndex(data, within)
        along <- ".simr_repl"
    }

    x <- with(data, get(along))
    targets <- sort(unique(x))

    # refactor into new function?
    if(along == ".simr_repl") {

        xlab <- str_c("number of observations within ", within)
        xval <- seq_along(targets)

    } else {

        if(is.factor(x)) {

            xlab <- str_c("number of levels in ", along)
            xval <- seq_along(targets)

        } else {

            xlab <- str_c("largest value of ", along)
            xval <- targets
        }
    }

    if(missing(breaks)) {

        breaks <- tidySeq(getSimrOption("pcmin"), length(targets), getSimrOption("pcmax"))
    }
    xval <- xval[breaks]

    ss_list <- llply(breaks, function(z) x %in% head(targets, z))

    msg <- if(along==".simr_repl") {
        str_c("Calculating power at ", length(ss_list), " sample sizes within ", within)
    } else str_c("Calculating power at ", length(ss_list), " sample sizes along ", along)

    if(getSimrOption("progress")) message(msg)

    simulations <- maybe_llply(seq_len(nsim), function(.) doSim(sim), .text="Simulating")

    psF <- function(ss) {

        powerSim(
            fit=fit,
            test=test,
            sim=iter(simulations$value),
            fitOpts=c(list(subset=ss), fitOpts),
            testOpts=testOpts, simOpts=simOpts
        )
    }

    psList <- maybe_llply(ss_list, psF, .progress=counter_simr(), .text="powerCurve", .extract=TRUE)

    # END TIMING
    timing <- proc.time() - start

    z <- list(
        ps = psList$value,
        alpha = getSimrOption("alpha"),
        text = attr(test, "text")(fit, sim),
        along = along,
        warnings = psList$warnings,
        errors = psList$errors,
        nlevels = breaks,
        simrTag = observedPowerWarning(sim),
        xlab = xlab,
        xval = xval,
        timing = timing
    )

    rval <- structure(z, class="powerCurve")

    .simrLastResult $ lastResult <- rval

    return(rval)
}

#
# Function to calculate tidy subset breaks
#

tidySeq <- function(from=getSimrOption("pcmin"), to, maxLength=getSimrOption("pcmax")) {

    if(to < from) return(to)

    if(to - from + 1 <= maxLength) return(seq(from, to))

    round(seq(from, to, length=maxLength))
}
