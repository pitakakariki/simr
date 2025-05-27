#
# print, summary, and confint for powerSim and powerCurve objects
#
# nb: plot function are in powerPlot.R
#
#' Report simulation results
#'
#' Describe and extract power simulation results
#'
#' @param x a \code{\link{powerSim}} or \code{\link{powerCurve}} object
#' @param object a \code{\link{powerSim}} or \code{\link{powerCurve}} object
#' @param parm currently ignored, included for S3 compatibility with \code{\link[=confint]{stats::confint}}
#' @param alpha the significance level for the statistical test (default is that used in the call to \code{powerSim}).
#' @param level confidence level for power estimate
#' @param method method to use for computing binomial confidence intervals (see \code{\link[=binom.confint]{binom::binom.confint()}})
#' @param ... additional arguments to pass to \code{\link[=binom.confint]{binom::binom.confint()}}
#'
#'   \code{alpha} refers to the threshold for an effect being significant and
#'   thus directly determines the point estimate for the power calculation.
#'   \code{level} is the confidence level that is calculated for this point
#'   evidence and determines the width/coverage of the confidence interval for
#'   power.
#' @seealso  \code{\link[=binom.confint]{binom::binom.confint}}, \code{\link{powerSim}}, \code{\link{powerCurve}}
#' @export
print.powerSim <- function(x, alpha=x$alpha, level=0.95, ...) {

    cat(x$text)
    cat(paste0(", (",level*100,"% confidence interval):\n      "))
    printerval(x, alpha=alpha, level=level, ...)
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

    cat("alpha = ", alpha, ", nrow = ", x$nrow, sep="")
    cat("\n")

    time <- x$timing['elapsed']
    cat(sprintf("\nTime elapsed: %i h %i m %i s\n", floor(time/60/60), floor(time/60) %% 60, floor(time) %% 60))

    if(x$simrTag) cat("\nnb: result might be an observed power calculation\n")
}

#' @rdname print.powerSim
#' @export
print.powerCurve <- function(x, ...) {

  cat(x$text)
  cat(", (95% confidence interval),\n")

  #l_ply(x$pa, function(x) {printerval(x);cat("\n")})
  cat("by ", x$xlab, ":\n", sep="")
  for(i in seq_along(x$ps)) {

    cat(sprintf("%7s: ", x$xval[i]))
    printerval(x$ps[[i]], ...)
    cat(" -", x$ps[[i]]$nrow, "rows")
    cat("\n")
  }

  time <- x$timing['elapsed']
  cat(sprintf("\nTime elapsed: %i h %i m %i s\n", floor(time/60/60), floor(time/60) %% 60, floor(time) %% 60))
}

#' @rdname print.powerSim
#' @export
summary.powerSim <- function(object, alpha=object$alpha, level=0.95, method=getSimrOption("binom"), ...) {

    x <- sum(object$pval < alpha, na.rm=TRUE)
    n <- object$n

    power <- binom.confint(x, n, conf.level=level, methods=method, ...)[c("mean", "lower", "upper")]

    rval <- cbind(successes=x, trials=n, power)

    class(rval) <- c("summary.powerSim", class(rval))

    return(rval)
}

#' @rdname print.powerSim
#' @export
summary.powerCurve <- function(object, alpha=object$alpha, level=0.95, method=getSimrOption("binom"), ...) {

    rval <- do.call(rbind, lapply(object$ps, summary, alpha=alpha, level=level, method=method))
    rval <- cbind(nrow=sapply(object$ps, `[[`, "nrow"), nlevels=object$nlevels, rval)

    class(rval) <- c("summary.powerCurve", class(rval))

    return(rval)
}

#' @rdname print.powerSim
#' @export
confint.powerSim <- function(object, parm, level=0.95, method=getSimrOption("binom"), alpha=object$alpha, ...) {

    x <- sum(object$pval < alpha, na.rm=TRUE)
    n <- object$n

    rval <- binom.confint(x, n, conf.level=level, methods=method, ...)[c("lower", "upper")]

    rval <- as.matrix(rval)
    levelNames <- paste(format(100 * c((1-level)/2, 1-(1-level)/2), trim=TRUE, scientific=FALSE, digits=3), "%")
    dimnames(rval) <- list("power", levelNames)

    return(rval)
}

#' @rdname print.powerSim
#' @export
confint.powerCurve <- function(object, parm, level=0.95, method=getSimrOption("binom"), ...) {

    rval <- do.call(rbind, lapply(object$ps, confint, ...))
    row.names(rval) <- object$xval

    return(rval)
}

printerval <- function(object, alpha=object$alpha, level=0.95, method=getSimrOption("binom"), ...) {

    x <- sum(object$pval < alpha, na.rm=TRUE)
    n <- object$n

    # check for NA
    if(is.na(x) || is.na(n) || (n==0)) {

        cat("<NA>")
        return()
    }

    interval <- binom.confint(x, n, level, method, ...)[c("mean", "lower", "upper")]
    cat(as.percentage(interval))
}

# vectorised, w/ % sign
as.percentage1 <- function(x) ifelse(is.na(x), "<NA>", ifelse(x==1, "100.0%", sprintf("%5.2f%%", 100*x)))

# vecorised, no % sign
as.percentage2 <- function(x) ifelse(is.na(x), "<NA>", ifelse(x==1, "100.0", sprintf("%5.2f", 100*x)))

# vectorised x.xx% (x.xx, x.xx)
as.percentage3 <- function(x, y, z) str_c(as.percentage1(x), " (", as.percentage2(y), ", ", as.percentage2(z), ")")

#
as.percentage <- function(x) as.percentage3(x[1], x[2], x[3])
