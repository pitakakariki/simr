estimate <- function(object, level, method) UseMethod("estimate")

estimate.poweranalysis <- function(object, level=0.95, method=getSimrOption("binom")) {
    
    binom.confint(object$x, object$n, level, method)[c("mean", "lower", "upper")]
}

estimate.powerCurve <- function(object, level=0.95) {
    
    
    
    
}

printerval <- function(z, method=.simrOptions$binom, ...) {
    
    # check for NA
    if(any(is.na(z))) {
        
        cat("<NA>")
        return()
    }
    
    interval <- 100 * binom.confint(z$x, z$n, 0.95, method)[c("mean", "lower", "upper")]
    with(interval, cat(sprintf("%6.2f%% (%6.2f, %6.2f)", mean, lower, upper)))
}