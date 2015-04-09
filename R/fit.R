#' Fit model to a new response.
#'
#' This is normally an internal function, but it can be overloaded to extend \code{simr} to other packages.
#'
#' @param y new values for the response variable (vector or matrix depending on the model).
#' @param model a previously fitted model object.
#' @param subset boolean vector specifying how much of the data to use. If missing, the model is fit to all
#'     the data. This argument needs to be implemented for \code{\link{powerCurve}} to work.
#'
#' @return a fitted model object.
#'
#' @export
doFit <- function(y, model, subset) UseMethod('doFit', model)

#' @export
doFit.lm <- function(y, model, subset) {

    newData <- model$model
    responseName <- model$call$formula[[2]]
    newData[[responseName]] <- y

    newData <- newData[subset, ]

    model$call[["data"]] <- quote(newData)

    rval <- eval(model$call)

    return(rval)
}

#' @export
doFit.lmerMod <- function(y, model, subset) {

    # need to have tests
    #stopifnot(is(model, "merModLmerTest"))

    newData <- getData(model)
    responseName <- formula(model)[[2]]
    newData[[responseName]] <- y

    newData <- newData[subset, ]

    newCall <- getCall(model)
    newCall[["data"]] <- quote(newData)
    newCall[[1]] <- quote(lmer) ## why?

    e <- new.env(parent=environment(formula(newCall)))
    attr(newCall$formula, ".Environment") <- e
    assign("newData", newData, envir=e)

    #if(getSimrOption("lmerhint")) newCall[["start"]] <- getME(model, "theta")

    rval <- eval(newCall)

    ##TODO## do this properly. maybe an lme4 bugfix
    #environment(attr(rval@frame, "formula")) <- as.environment(newData)

    return(rval)
}

#' @export
doFit.default <- function(y, model, subset) {

    # need to have tests
    #stopifnot(is(model, "merModLmerTest"))

    newData <- getData(model)
    responseName <- formula(model)[[2]]
    newData[[responseName]] <- y

    newData <- newData[subset, ]

    newCall <- getCall(model)
    newCall[["data"]] <- quote(newData)
    #newCall[[1]] <- quote(lmer) ## why?

    e <- new.env(parent=environment(formula(newCall)))
    attr(newCall$formula, ".Environment") <- e
    assign("newData", newData, envir=e)

    #if(getSimrOption("lmerhint")) newCall[["start"]] <- getME(model, "theta")

    rval <- eval(newCall)

    ##TODO## do this properly. maybe an lme4 bugfix
    #environment(attr(rval@frame, "formula")) <- as.environment(newData)

    return(rval)
}
