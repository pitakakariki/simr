#' @export
doFit.glm <- function(y, fit, subset, ...) {

    newData <- getData(fit)
    responseName <- as.character(formula(fit)[[2]])

    # hack for binomial
    if(responseName[1] == "cbind") {

        responseName <- responseName[2]
        if(is.matrix(y)) y <- y[, responseName]
    }

    newData[[responseName]] <- y

    newData <- newData[subset, ]

    fit$call[["data"]] <- quote(newData)

    rval <- eval(fit$call)

    return(rval)
}


