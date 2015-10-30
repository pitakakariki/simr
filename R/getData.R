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

    if(is(object, "merMod")) return(object@frame)
    if(is(object, "lm")) return(object$model)

    #
    # 3rd choice: Use the `data` argument
    #

    dataName <- as.character(getCall(object)$data)
    E <- environment(formula(object))

    if(length(dataName) > 0) return(get(dataName, envir=E))

    #
    # If none of the above worked:
    #

    stop("Couldn't find object's data.")
}

#' @rdname getData
#' @export
`getData<-` <- function(object, value) {

    attr(object, "newData") <- value
    return(object)
}
