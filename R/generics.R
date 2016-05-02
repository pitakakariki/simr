#
# summary and confint for powerSim and powerCurve objects
#
# nb: plot function are in powerPlot.R
#

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

    cat("alpha = ", x$alpha, ", nrow = ", x$nrow, sep="")
    cat("\n")

    time <- x$timing['elapsed']
    cat(sprintf("\nTime elapsed: %i h %i m %i s\n", floor(time/60/60), floor(time/60) %% 60, floor(time) %% 60))

    if(x$simrTag) cat("\nnb: result might be an observed power calculation\n")
}

#' @export
print.powerCurve <- function(x, ...) {

  cat(x$text)
  cat(", (95% confidence interval),\n")

  #l_ply(x$pa, function(x) {printerval(x);cat("\n")})
  cat("by ", x$xlab, ":\n", sep="")
  for(i in seq_along(x$ps)) {

    cat(sprintf("%7i: ", x$xval[i]))
    printerval(x$ps[[i]], ...)
    cat(" -", x$ps[[i]]$nrow, "rows")
    cat("\n")
  }

  time <- x$timing['elapsed']
  cat(sprintf("\nTime elapsed: %i h %i m %i s\n", floor(time/60/60), floor(time/60) %% 60, floor(time) %% 60))
}

#' @export
summary.powerSim <- function(object, level=0.95, pval=object$pval, method=getSimrOption("binom"), ...) {

    x <- object$x
    n <- object$n

    power <- binom.confint(x, n, level, method)[c("mean", "lower", "upper")]

    rval <- cbind(successes=x, trials=n, power)

    class(rval) <- c("summary.powerSim", class(rval))

    return(rval)
}

#' @export
summary.powerCurve <- function(object, level=0.95, method=getSimrOption("binom"), ...) {


    rval <- ldply(object$ps, summary, level=level, method=method)
    rval <- cbind(nlevels=object$nlevels, rval)

    class(rval) <- c("summary.powerCurve", class(rval))

    return(rval)
}

#' @export
confint.powerSim <- function(object, parm, level=0.95, method=getSimrOption("binom"), ...) {

    x <- object$x
    n <- object$n

    rval <- binom.confint(x, n, conf.level=level, methods=method, ...)[c("lower", "upper")]

    rval <- as.matrix(rval)
    levelNames <- paste(format(100 * c((1-level)/2, 1-(1-level)/2), trim=TRUE, scientific=FALSE, digits=3), "%")
    dimnames(rval) <- list("power", levelNames)

    return(rval)
}


#' @export
confint.powerCurve <- function(object, parm, level=0.95, method=getSimrOption("binom"), ...) {

    rval <- do.call(rbind, lapply(object$ps, confint))
    row.names(rval) <- object$xval

    return(rval)
}

printerval <- function(z, method=getSimrOption("binom")) {

    # check for NA
    if(is.na(z$x) || is.na(z$n) || (z$n==0)) {

        cat("<NA>")
        return()
    }

    interval <- 100 * binom.confint(z$x, z$n, 0.95, method)[c("mean", "lower", "upper")]
    #with(interval, cat(sprintf("%6.2f%% (%6.2f, %6.2f)", mean, lower, upper)))

    #cat(as.percentage3(summary(z)[c("mean", "lower", "upper")]))

    sz <- summary(z)

    cat(as.percentage3(sz["mean"], sz["lower"], sz["upper"]))
}

# vectorised, w/ % sign
as.percentage1 <- function(x) ifelse(is.na(x), "<NA>", ifelse(x==1, "100.0%", sprintf("%5.2f%%", 100*x)))

# vecorised, no % sign
as.percentage2 <- function(x) ifelse(is.na(x), "<NA>", ifelse(x==1, "100.0", sprintf("%5.2f", 100*x)))

# x.xx% (x.xx, x.xx)
as.percentage3 <- function(x, y, z) str_c(as.percentage1(x), " (", as.percentage2(y), ", ", as.percentage2(z), ")")


