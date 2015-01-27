wrapdoFit <- function(y, model, subset) {

  f <- function() UseMethod('doFit', model)

  tryCatch(f(), warning=., error=.)
}

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

#' Fit an lme4 model to new data.
#'
#' @export
#'
doFit.lmerMod <- function(y, model, subset) {

    # need to have tests
    #stopifnot(is(model, "merModLmerTest"))

    newData <- model@frame
    responseName <- model@call$formula[[2]]
    newData[[responseName]] <- y

    newData <- newData[subset, ]

    newCall <- model@call
    newCall[["data"]] <- newData
    newCall[[1]] <- quote(lmer)

    if(getSimrOption("lmerhint")) newCall[["start"]] <- getME(model, "theta")

    rval <- eval(newCall)

    ##TODO## do this properly. maybe an lme4 bugfix
    #environment(attr(rval@frame, "formula")) <- as.environment(newData)

    return(rval)
}


