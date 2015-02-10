#' Get an object's data.
#'
#' Get the data associated with a model object.
#'
#' @export
#'
#' @param object a mixed-effects model (merMod) object.
#'
#' @details
#'
#' Looks for data in the following order:
#'
#' \enumerate{
#'  \item{The \code{data} argument specified in the model's \code{call}.}
#'  \item{The \code{data.frame} stored in the \code{frame} slot of \code{object}.}
#' }
#'
#' @return
#'
#' A \code{data.frame} with the required data.
#'
#' @examples
#'
#' a <- lmer(Carbon ~ Year + (Year | Cluster), kiwifruit)
#' X <- getData(a)
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