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
    if(!is.character(responseName)) responseName <- deparse(responseName)
    responseName <- make.names(responseName)

    newData <- getData(fit)
    newData[[responseName]] <- y

    newData <- newData[subset, ]

    newCall <- getCall_(fit)
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

#
# Workaround for glmer.nb see https://github.com/lme4/lme4/issues/404
#

getCall_ <- function(x) {

    if(isNB(x)) return(getCallNB(x))

    getCall(x)
}

isNB <- function(x) {

    is(x, "glmerMod") && !is.na(getME(x, "glmer.nb.theta"))
}

getCallNB <- function(x) {

    oldCall <- getCall(x) # this will be the inner loop glmer call

    oldCall[[1]] <- quote(glmer.nb)
    oldCall$family <- NULL

    return(oldCall)
}

#
# NB: binomial responses using cbind should now be handled by the response name / gensym stuff
#

# #' @export
# doFit.glmerMod <- function(y, fit, subset, ...) {
#
#     # need to have tests
#     #stopifnot(is(model, "merModLmerTest"))
#
#     newData <- getData(fit)
#     responseName <- as.character(as.formula(fit)[[2]])
#
#     # hack for binomial
#     if(responseName[1] == "cbind") {
#
#         responseName <- responseName[2]
#          if(is.matrix(y)) y <- y[, responseName]
#     }
#
#     newData[[responseName]] <- y
#
#     newData <- newData[subset, ]
#
#     newCall <- fit@call
#     newCall[["data"]] <- newData
#     if("control" %in% names(newCall)) newCall[["control"]] <- NULL
#     newCall[[1]] <- quote(glmer)
#
#     #if(getSimrOption("lmerhint")) newCall[["start"]] <- getME(model, "theta")
#
#     rval <- eval(newCall)
#
#     ##TODO## do this properly. maybe an lme4 bugfix
#     #environment(attr(rval@frame, "formula")) <- as.environment(newData)
#
#     return(rval)
# }
#
# #' @export
# doFit.glm <- function(y, fit, subset, ...) {
#
#     newData <- getData(fit)
#     responseName <- as.character(formula(fit)[[2]])
#
#     # hack for binomial
#     if(responseName[1] == "cbind") {
#
#         responseName <- responseName[2]
#         if(is.matrix(y)) y <- y[, responseName]
#     }
#
#     newData[[responseName]] <- y
#
#     newData <- newData[subset, ]
#
#     fit$call[["data"]] <- quote(newData)
#
#     rval <- eval(fit$call)
#
#     return(rval)
# }
