
# End result needs to be a merMod object suitable for simulate.merMod

# Therefore need to change:

# ?

# ?

# ?


# In order to set those, need to calculate:

# new X matrix

# new Z matrix

extendList <- function(object, along='Year', from, to) {

    sl <- seqList(from, to)
    rval <- lapply(sl, function(.) extend(object, along, values=.))

    return(rval)
}

seqList <- function(from, to) {

    lapply(tail(seq(from, to), -1), seq, from=from)

}

#' Extend a longitudinal model.
#'
#' This method increases the sample size for a model.
#'
#' @param object a linear mixed-effects model (\code{lmerMod}) object to extend.
#' @param along the name of an explanatory variable. This variable will have its number of levels extended.
#' @param n the levels of the explanatory variable will be replaced by \code{1:n}.
#' @param values alternatively, specify a new set of levels for the explanatory variable.
#'
#' @export
#'
#' @examples
#' fm <- lmer(y ~ x + (1|g), data=example)
#' fmx <- extend(fm, along='x', n=20)
#'
extend <- function(object, along='x', n=length(values), values=seq_len(n)) {

    if(missing(n) && missing(values)) stop('Extended values not specified.')

    extendData <- extendFrame(object, along, values)

    newCall <- object @ call
    newCall$data <- quote(extendData)
    #newCall$control <- quote(lmerCopy(object))
    newCall$control <- lmerCopy(object)

    # we don't want convergence warnings.
    newCall$control $ checkConv $ check.conv.grad $ action <- "ignore"
    newCall$control $ checkConv $ check.conv.singular $ action <- "ignore"
    newCall$control $ checkConv $ check.conv.hess $ action <- "ignore"

    newCall$na.action <- quote(na.pass)

    newObject <- eval(newCall)

    #newObject @ call <- match.call()
    newObject @ call <- newCall

    #
    # ensure parameters are the same in the new model
    #

    # beta
    fixef(newObject) <- fixef(object)

    # sigma
    newObject @ devcomp $ cmp <- object @devcomp $ cmp

    # ranef
    len1 <- length(object @ pp $ delu)
    len2 <- length(newObject @ pp $ delu)
    ranefCheck <- (len1 == len2)

    if(ranefCheck) {
        newObject @ pp $ setDelu(object @ pp $ delu)
    } else {
        ##TODO## set annotation no use.u
    }

    # keep a copy of the original
    attr(newObject, 'original') <- object

    return(newObject)
}

extendFrame <- function(object, along, values) {

    # reduce to one measurement
    one_X <- reduceFrame(object, along)

    # repeat N times
    f <- function(value) {

        one_X[[along]] <- value
        return(one_X)
    }

    X <- do.call(rbind, lapply(values, f))

    return(X)
}

#' Reduce a longitudinal model to a single time period.
#'
#'
#'
#'
#'
#'
#'
reduceFrame <- function(object, along, level=X[[along]][1]) {

    X <- getData(object)
    s <- (X[[along]] == level)

    X[s, ]
}

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

    dataExpr <- object @ call $ data

    if(length(dataExpr)) {

        # option 1: data from global environment
        #get(dataName, envir=globalenv())
        eval(dataExpr, env=globalenv())
    } else {

        # option 2: frame from object
        return(object @ frame)
    }
}

# rebalance?

