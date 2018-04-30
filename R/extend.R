#' Extend a longitudinal model.
#'
#' This method increases the sample size for a model.
#'
#' @param object a fitted model object to extend.
#' @param along the name of an explanatory variable. This variable will have its number of levels extended.
#' @param within names of grouping variables, separated by "+" or ",". Each combination of groups will be
#'               extended to \code{n} rows.
#' @param n number of levels: the levels of the explanatory variable will be replaced by \code{1,2,3,..,n} for a
#'          continuous variable or \code{a,b,c,...,n} for a factor.
#' @param values alternatively, you can specify a new set of levels for the explanatory variable.
#'
#' @details
#'
#' \code{extend} takes "slices" through the data for each unique value of the extended variable.
#' An extended dataset is built from \code{n} slices, with slices duplicated if necessary.
#'
#' @return
#'
#' A copy of \code{object} suitable for \code{\link{doSim}} with an extended dataset attached as
#' an attribute named \code{newData}.
#'
#' @examples
#' fm <- lmer(y ~ x + (1|g), data=simdata)
#' nrow(example)
#' fmx1 <- extend(fm, along="x", n=20)
#' nrow(getData(fmx1))
#' fmx2 <- extend(fm, along="x", values=c(1,2,4,8,16))
#' nrow(getData(fmx2))
#'
#' @export
extend <- function(object, along, within, n, values) UseMethod("extend", object)

#' @export
extend.data.frame <- function(object, along, within, n, values) {

    if(missing(n) && missing(values)) stop("Extended values not specified.")

    if(!missing(along) && !missing(within)) stop("Only one of along and within may be used.")

    if(!missing(within)) {

        object <- addReplicateIndex(object, within)
        along <- ".simr_repl"
    }

    a <- is.factor(object[[along]])
    b <- along %in% all.vars(nobars(formula(object)[[length(formula(object))]]))

    if(missing(values)) {

        if(a) {

            values <- character(n)
            suppressWarnings(values[] <- letters)
            values <- make.unique(values)

        } else {

            values <- seq_len(n)

        }
    }

    ## NB this is where the extension logic gets defined
    ## e.g. if values=c("a", "b", "c"), and unique(along)=c(1, 2)
    ## then oldvalues=c(1, 2, 1)

    # repeat unique `along` values, length(values) times.
    oldValues <- values
    suppressWarnings(oldValues[] <- as.character(unique(object[[along]])))

    # repeat N times
    f <- function(value, oldValue) {

        #one_X <- reduce(object, along, oldValue)
        one_X <- object[(object[[along]] == oldValue), , drop=FALSE]
        if(a) levels(one_X[[along]]) <- values

        one_X[[along]][] <- value
        return(one_X)
    }

    X <- do.call(rbind, mapply(f, values, oldValues, SIMPLIFY=FALSE))

    # cleanup
    X$.simr_repl <- NULL

    # copy contrast attributes
    for(j in seq_along(X)) {

        C <- attr(object[[j]], "contrasts")

        if(!is.null(C)) contrasts(X[[j]]) <- C
    }

    return(X)

}

#' @export
extend.default <- function(object, along, within, n, values) {

    # Sanity checks

    if(length(unique(weights(object))) > 1  && !cbindResponse(object)) {

        warning("Non-uniform weights are not supported")
    }

    if(missing(n) && missing(values)) {

        stop("Extended values not specified.")
    }

    if(missing(within)) {

        if(missing(along)) along <- getDefaultXname(object)

        a <- is.factor(getData(object)[[along]])
        b <- along %in% all.vars(nobars(formula(object)[[length(formula(object))]]))

        if(a && b) stop("Cannot extend along a fixed factor.")
    }

    # Attach an extended data.frame

    newData <- extend(getData(object), along, within, n, values)

    getData(object) <- newData

    return(object)
}

#' @export
extend.lm <- function(object, along, within, n, values) {

    newData <- extend(getData(object), along, within, n, values)

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


