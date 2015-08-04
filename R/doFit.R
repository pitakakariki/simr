#' Fit model to a new response.
#'
#' This is normally an internal function, but it can be overloaded to extend \code{simr} to other packages.
#'
#' @param y new values for the response variable (vector or matrix depending on the model).
#' @param fit a previously fitted model object.
#' @param subset boolean vector specifying how much of the data to use. If missing, the model is fit to all
#'     the data. This argument needs to be implemented for \code{\link{powerCurve}} to work.
#'
#' @return a fitted model object.
#'
#' @export
doFit <- function(y, fit, subset) UseMethod('doFit', fit)

#' @export
doFit.default <- function(y, fit, subset, ...) {

    # need to have tests
    #stopifnot(is(fit, "merModLmerTest"))

    newData <- getData(fit)
    responseName <- formula(fit)[[2]]
    newData[[responseName]] <- y

    newData <- newData[subset, ]

    newCall <- getCall(fit)
    newCall[["data"]] <- quote(newData)
    #newCall[[1]] <- quote(lmer) ## why?

    e <- new.env(parent=environment(formula(newCall)))
    attr(newCall$formula, ".Environment") <- e
    assign("newData", newData, envir=e)

    #if(getSimrOption("lmerhint")) newCall[["start"]] <- getME(fit, "theta")

    opts <- list(...)
    newCall[names(opts)] <- opts

    rval <- eval(newCall)

    ##TODO## do this properly. maybe an lme4 bugfix
    #environment(attr(rval@frame, "formula")) <- as.environment(newData)

    return(rval)
}

#' @export
doFit.function <- function(y, fit, subset, ...) {

    ss <- "subset" %in% names(formals(fit))

    if(!ss & !missing(subset)) stop("The supplied function has no subset argument")

    if(!ss) {

        fit(y, ...)

    } else {

        fit(y, subset=subset, ...)
    }
}
