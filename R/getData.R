#' Get an object's data.
#'
#' Get the data associated with a model object.
#'
#' @export
#'
#' @param object a fitted model object (e.g. an object of class \code{merMod} or \code{lm}).
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
#' lm1 <- lmer(y ~ x + (1|g), data=example)
#' X <- getData(lm1)
#'
getData <- function(object) {

    #
    # 1st choice: has simr set a `newData` attribute?
    #

    newData <- attr(object, "newData")
    if(!is.null(newData)) return(newData)

    #
    # 2nd choice: Use the `data` argument
    #

    dataName <- as.character(getCall(object)$data)
    E <- environment(formula(object))

    if(length(dataName) > 0) return(get(dataName, envir=E))

    #
    # If none of the above worked:
    #

    stop("Couldn't find object's data.")
}