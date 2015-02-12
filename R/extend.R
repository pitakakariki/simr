#' Extend a longitudinal model.
#'
#' This method increases the sample size for a model.
#'
#' @param object a fitted model object to extend.
#' @param along the name of an explanatory variable. This variable will have its number of levels extended.
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
#' fm <- lmer(y ~ x + (1|g), data=example)
#' nrow(example)
#' fmx <- extend(fm, along='x', n=20)
#' nrow(getData(fmx))
#'
#' @export
extend <- function(object, along, n, values) UseMethod('extend', object)

extend.merMod <- function(object, along, n, values) {

    newData <- extendData(object, along, n, values)

    attr(object, "newData") <- newData

    return(object)
}

extend.lm <- function(object, along, n, values) {

    newData <- extendData(object, along, n, values)

    newCall <- getCall(object)
    newCall$data <- quote(newData)

    newObject <- eval(newCall)

    # beta and sigma
    coef(newObject) <- coef(object)
    suppressWarnings(
        sigma(newObject) <- sigma(object)
    ) # In summary.lm(object) : essentially perfect fit: summary may be unreliable

    # less likely to have problems if the data's kept here
    attr(newObject, 'newData') <- newData

    return(newObject)
}


#
# Reduce a longitudinal dataset to a single time period / group / etc.
#
reduceData <- function(object, along, level=X[[along]][1]) {

    X <- getData(object)
    s <- (X[[along]] == level)

    X[s, ]
}

#
# Build a larger dataset from reduced versions.
#
extendData <- function(object, along, n, values) {

    if(missing(n) && missing(values)) stop('Extended values not specified.')

    if(missing(along)) along <- getDefaultXname(object)

    a <- is.factor(getData(object)[[along]])
    b <- along %in% all.vars(nobars(formula(object)[[length(formula(object))]]))

    if(a && b) stop("Cannot extend along a fixed factor.")

    if(missing(values)) {

        if(a) {

            values <- character(n)
            suppressWarnings(values[] <- letters)
            values <- make.unique(values)

        } else {

            values <- seq_len(n)

        }
    }

    # reduce to one measurement
    one_X <- reduceData(object, along)
    levels(one_X[[along]]) <- values

    # repeat N times
    f <- function(value) {

        one_X[[along]][] <- value
        return(one_X)
    }

    X <- do.call(rbind, lapply(values, f))

    return(X)
}

