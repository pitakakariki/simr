doFit <- function(y, model, subset) UseMethod('doFit', model)

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
    newCall[[1]] <- quote(lmer)

    e <- new.env(parent=environment(formula(newCall)))
    attr(newCall$formula, ".Environment") <- e
    assign("newData", newData, envir=e)

    #if(getSimrOption("lmerhint")) newCall[["start"]] <- getME(model, "theta")

    rval <- eval(newCall)

    ##TODO## do this properly. maybe an lme4 bugfix
    #environment(attr(rval@frame, "formula")) <- as.environment(newData)

    return(rval)
}


