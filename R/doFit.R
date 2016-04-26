#' Fit model to a new response.
#'
#' This is normally an internal function, but it can be overloaded to extend \code{simr} to other packages.
#'
#' @param y new values for the response variable (vector or matrix depending on the model).
#' @param fit a previously fitted model object.
#' @param subset boolean vector specifying how much of the data to use. If missing, the model is fit to all
#'     the data. This argument needs to be implemented for \code{\link{powerCurve}} to work.
#' @param ... additional options.
#'
#' @return a fitted model object.
#'
#' @export
doFit <- function(y, fit, subset, ...) UseMethod('doFit', fit)

#' @export
doFit.default <- function(y, fit, subset, ...) {

    ## nb: `responseName` might be e.g. log(z)
    ## in this case, need a gensym using make.names
    ## a) in newData
    ## b) replacing the response in fit's formula

    responseName <- formula(fit)[[2]]
    if(!is.character(responseName)) responseName <- deparse(responseName)
    responseName <- make.names(responseName)

    newData <- getData(fit)
    newData[[responseName]] <- y

    newData <- newData[subset, ]

    newCall <- getCall(fit)
    newCall[["formula"]][[2]] <- as.symbol(responseName)
    newCall[["data"]] <- quote(newData)

    e <- new.env(parent=environment(formula(newCall)))
    attr(newCall$formula, ".Environment") <- e
    assign("newData", newData, envir=e)

    opts <- list(...)
    newCall[names(opts)] <- opts

    rval <- eval(newCall)

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
