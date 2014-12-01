estimate <- function(object, level, method) UseMethod("estimate")

estimate.poweranalysis <- function(object, level=0.95, pval=object$pval, method=getSimrOption("binom")) {

    x <- object$x
    n <- object$n

    binom.confint(x, n, level, method)[c("mean", "lower", "upper")]
}

estimate.powerCurve <- function(object, level=0.95, method=getSimrOption("binom")) {


    ldply(object$ps, estimate, level=level, method=method)

}

printerval <- function(z, method=getSimrOption("binom")) {

    # check for NA
    if(any(is.na(z))) {

        cat("<NA>")
        return()
    }

    interval <- 100 * binom.confint(z$x, z$n, 0.95, method)[c("mean", "lower", "upper")]
    #with(interval, cat(sprintf("%6.2f%% (%6.2f, %6.2f)", mean, lower, upper)))

    cat(as.percentage3(estimate(z)))
}

printerval2 <- function(z, method=.simrOptions$binom, ...) {

    # check for NA
    if(any(is.na(z))) {

        cat("<NA>")
        return()
    }

    interval <- 100 * binom.confint(z$x, z$n, 0.95, method)[c("mean", "lower", "upper")]
    with(interval, cat(sprintf("%6.2f%% (%6.2f, %6.2f)", mean, lower, upper)))
}

as.percentage <- function(x) ifelse(x==1, "100.0%", sprintf("%5.2f%%", 100*x))
as.percentage2 <- function(x) ifelse(x==1, "100.0", sprintf("%5.2f", 100*x))
as.percentage3 <- function(x) str_c(as.percentage(x[1]), " (", as.percentage2(x[2]), ", ", as.percentage2(x[3]), ")")


