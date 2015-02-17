#' Calculate the power for an analysis at a range of levels.
#'
#' This function runs \code{powerSim} for a series of designs with a range of sample sizes.
#'
#' @param fit a fitted model object (see \code{\link{doFit}}).
#' @param test specify the test to perform. By default, the first fixed effect in \code{fit} will be tested.
#'     (see: \link{tests}).
#' @param sim an object to simulate from. By default this is the same as \code{fit} (see \code{\link{doSim}}).
#' @param along the name of an explanatory variable. This variable will have its number of levels varied.
#' @param nsim the number of simulations to run.
#' @param alpha the significance level for the statistical test. Defaults to 0.05.
#' @param breaks number of levels of the variable specified by \code{along} at each point on the power curve.
#' @param seed specify a random number generator seed, for reproducible results.
#' @param ... any additional arguments are passed on to \code{\link{doFit}}.
#'
#' @examples
#' \dontrun{
#' fm <- lmer(y ~ x + (1|g), data=example)
#' pc1 <- powerCurve(fm)
#' pc2 <- powerCurve(fm, breaks=c(4,6,8,10))
#' print(pc)
#' plot(pc)
#' }
#'
#' @export
powerCurve <- function(

    fit,
    test = fixed(getDefaultXname(fit)),
    sim = fit,

    along = getDefaultXname(fit),
    nsim = getSimrOption("nsim"),
    alpha = 0.05,

    breaks,
    seed,

    ...

    ) {

    # START TIMING
    timing <- system.time({

    if(!missing(seed)) set.seed(seed)

    # auto subsetting
    x <- with(getData(fit), get(along))
    targets <- unique(x)

    if(missing(breaks)) {

        breaks <- tidySeq(getSimrOption("pcmin"), length(targets), getSimrOption("pcmax"))
    }

    ss_list <- llply(breaks, function(z) x %in% head(targets, z))

    msg <- str_c("Calculating power at ", length(ss_list), " sample sizes for ", along)
    message(msg)

    simulations <- maybe_llply(seq_len(nsim), function(.) doSim(sim), .text="Simulating")

    psF <- function(ss) powerSim(fit=fit, test=test, sim=iter(simulations$value), nsim=nsim, subset=ss, ...)
    psList <- maybe_llply(ss_list, psF, .progress=counter_simr(), .text="powerCurve", .extract=TRUE)

    z <- list(
        ps = psList$value,
        pval = alpha,
        text = attr(test, "text"),
        along = along,
        warnings = psList$warnings,
        errors = psList$errors,
        nlevels = breaks
    )

    rval <- structure(z, class="powerCurve")

    .SIMRLASTRESULT <<- rval

    })
    # END TIMING

    rval $ timing <- timing

    return(rval)
}

#' @export
print.powerCurve <- function(x, ...) {

  cat(x$text)
  cat(", (95% confidence interval):\n")

  #l_ply(x$pa, function(x) {printerval(x);cat("\n")})
  cat("#levels for", x$along, "\n")
  for(i in seq_along(x$ps)) {

    cat(sprintf("%7i: ", x$nlevels[i]))
    printerval(x$ps[[i]], ...)
    cat("\n")
  }

  time <- x$timing['elapsed']
  cat(sprintf("\nTime elapsed: %i h %i m %i s\n", floor(time/60/60), floor(time/60) %% 60, floor(time) %% 60))
}

timed <- function(f, mode=c("attribute", "list")) {

  mode <- match.arg(mode)

  function(...) {

    timing <- system.time(rval <- eval.parent(substitute(f(...))), gcFirst=TRUE)

    if(mode == "list") rval$timing <- timing
    if(mode == "attribute") attr(rval, "timing") <- timing

    return(rval)
  }
}

#
# Function to calculate tidy subset breaks
#

tidySeq <- function(from, to, maxLength) {

    if(to - from + 1 <= maxLength) return(seq(from, to))

    round(seq(from, to, length=maxLength))
}
