#estimate <- function(object, level, method) UseMethod("estimate")

summary.powerSim <- function(object, level=0.95, pval=object$pval, method=getSimrOption("binom")) {

    x <- object$x
    n <- object$n

    power <- binom.confint(x, n, level, method)[c("mean", "lower", "upper")]

    rval <- cbind(successes=x, trials=n, power)

    class(rval) <- c("summary.powerSim", class(rval))

    return(rval)
}

summary.powerCurve <- function(object, level=0.95, method=getSimrOption("binom")) {


    rval <- ldply(object$ps, summary, level=level, method=method)
    rval <- cbind(nlevels=object$nlevels, rval)

    class(rval) <- c("summary.powerCurve", class(rval))

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

    cat(as.percentage3(summary(z)[c("mean", "lower", "upper")]))
}

as.percentage1 <- function(x) ifelse(x==1, "100.0%", sprintf("%5.2f%%", 100*x))
as.percentage2 <- function(x) ifelse(x==1, "100.0", sprintf("%5.2f", 100*x))
as.percentage3 <- function(x) str_c(as.percentage1(x[1]), " (", as.percentage2(x[2]), ", ", as.percentage2(x[3]), ")")

as.percentage <- function(x) ifelse(is.na(x), "<NA>", as.percentage3(x))


