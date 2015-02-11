#' Calculate the power for an analysis at a range of levels.
#'
#' This function runs \code{powerSim} for a series of designs with a range of sample sizes.
#'
#' @param fit a linear mixed model object.
#' @param nsim the number of simulations to run.
#' @param xname the name of the explanatory variable to be tested for significance.
#' @param along the name of an explanatory variable. This variable will have its number of levels varied.
#' @param sim an object to simulate from, by default this is the same as \code{fit}.
#' @param pval the significance level for the statistical test. Defaults to 0.05.
#' @param seed specify a random number generator seed, for reproducible results.
#'
#' @export
#'
#' @examples
#' fm <- lmer(y ~ x + (1|g), data=example)
#' pc <- powerCurve(fm, nsim=10)
#' print(pc)
#' \dontrun{
#' plot(pc)
#' }
#'
powerCurve <- function(

    fit,
    test = fixed(getDefaultXname(fit)),
    sim = fit,

    along = getDefaultXname(fit),
    nsim = getSimrOption("nsim"),
    alpha = 0.05,

    seed,

    ...

    ) {

    # START TIMING
    timing <- system.time({

    if(!missing(seed)) set.seed(seed)

    ##TODO## specify which subsets we cover

    # auto subsetting

    x <- with(getData(fit), get(along))
    targets <- unique(x)
    targets_ix <- tidyss(targets, fit)

    ss_list <- lapply(targets_ix, function(z) x %in% head(targets, z))

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
        nlevels = targets_ix
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

    timing <- system.time(rval <- eval.parent(substitute(f(...))), gc=TRUE)

    if(mode == "list") rval$timing <- timing
    if(mode == "attribute") attr(rval, "timing") <- timing

    return(rval)
  }
}

#
# Function to calculate tidy subsets
#

tidyss <- function(targets, fit) {

    minlevel <- 3 ## TODO replace with heuristic
    maxlevel <- length(targets)
    numlevels <- 10 ## TODO replace with simrOption

    # use every level if there aren't too many:
    if(maxlevel - minlevel + 1 <= numlevels) return(seq(minlevel, maxlevel))

    rval <- round(seq(minlevel, maxlevel, length=numlevels))

    return(rval)
}

