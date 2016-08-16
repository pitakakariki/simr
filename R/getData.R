#' Get an object's data.
#'
#' Get the data associated with a model object.
#'
#' @param object a fitted model object (e.g. an object of class \code{merMod} or \code{lm}).
#' @param value a new \code{data.frame} to replace the old one.
#'    The new data will be stored in the \code{newData} attribute.
#'
#' @details
#'
#' Looks for data in the following order:
#'
#' \enumerate{
#'  \item{The object's \code{newData} attribute, if it has been set by \code{simr}.}
#'  \item{The \code{data} argument of \code{getCall(object)}, in the environment of \code{formula(object)}.}
#' }
#'
#' @return
#'
#' A \code{data.frame} with the required data.
#'
#' @examples
#'
#' lm1 <- lmer(y ~ x + (1|g), data=simdata)
#' X <- getData(lm1)
#'
#' @export
getData <- function(object) {

    #
    # 1st choice: has simr set a `newData` attribute?
    #

    newData <- attr(object, "newData")
    if(!is.null(newData)) return(newData)

    #
    # 2nd choice: @frame for merMod, $model for lm.
    #

    # not clever enough? Breaks e.g. binomial?

    #if(is(object, "merMod")) return(object@frame)
    #if(is(object, "lm")) return(object$model)

    #
    # @nd choice: doFit inserts a whole data.frame into the call
    #

    dataCall <- maybe(getCall)(object)$value$data
    if(is(dataCall, "data.frame")) return(dataCall)

    #
    # 3rd choice: evaluate the `data` argument
    #

    #dataName <- as.character(dataCall)
    if(length(dataCall) > 0) return(eval(dataCall, envir=environment(formula(object))))

    #
    # If none of the above worked:
    #

    stop("Couldn't find object's data.")
}

#' @rdname getData
#' @export
`getData<-` <- function(object, value) UseMethod("getData<-", object)

#' @export
`getData<-.default` <- function(object, value) {

    attr(object, "newData") <- value
    return(object)
}

#' @export
`getData<-.lm` <- function(object, value) {

    newData <- value

    newCall <- getCall(object)
    newCall$data <- quote(newData)

    newObject <- eval(newCall)

    # beta and sigma
    coef(newObject) <- coef(object)
    suppressWarnings(
        sigma(newObject) <- sigma(object)
    ) # In summary.lm(object) : essentially perfect fit: summary may be unreliable

    # less likely to have problems if the data's kept here?
    # attr(newObject, 'newData') <- newData

    return(newObject)
}
