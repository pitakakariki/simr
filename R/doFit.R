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

    ## nb: `responseName` might be e.g. log(z) or cbind(z, 10-z)
    ## in this case, need a gensym using make.names
    ## a) in newData
    ## b) replacing the response in fit's formula

    responseName <- formula(fit)[[2]]

    # cbind response for binomial
    if(as.character(responseName)[1] == "cbind") {

        responseForm <- responseName

        responseName <- responseName[[2]]
        if(is.matrix(y)) y <- y[, 1]

    } else {

        if(!is.character(responseName)) responseName <- deparse(responseName)
        responseName <- make.names(responseName)

        responseForm <- as.symbol(responseName)
    }

    newData <- getData(fit)
    newData[[responseName]] <- y

    newData <- newData[subset, ]

    newCall <- getCall(fit)
    newCall[["formula"]][[2]] <- responseForm
    newCall[["data"]] <- quote(newData)

    if("weights" %in% names(newCall)) {

        N <- nrow(getData(fit))
        w <- weights(fit)
        if(length(w) != N) {

            if(length(unique(w)) != 1) stop("Non-uniform weights are not supported")
            w <- rep(w[1], N)
        }

        w <- w[subset]

        newCall[["weights"]] <- w
    }

    opts <- list(...)
    newCall[names(opts)] <- opts

    e <- new.env(parent=environment(formula(newCall)))
    attr(newCall$formula, ".Environment") <- e
    assign("newData", newData, envir=e)

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
