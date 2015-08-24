#' Generate simulated response variables.
#'
#' This is normally an internal function, but it can be overloaded to extend \code{simr} to other packages.
#'
#' @param object an object to apply a statistcal test to, usually a fitted model.
#' @param a test function, see \link{tests}.
#'
#' @return a vector containing simulated response values (or, for models  with a multivariate response such as
#'     binomial gl(m)m's, a matrix of simulated response values). Suitable as input for \code{\link{doFit}}.
#'
#' @export
doTest <- function(object, test, ...) UseMethod('doTest', object)

#' @export
doTest.default <- function(object, test, ...) {

    test <- wrapTest(test)

    pval <- test(object, ...)

    rval <- list(

        pval = pval,
        text = str_c("p-value", substring(attr(test, "text")(object, object), 6)),
        description = attr(test, "description")(object, object)
    )

    class(rval) <- "test"

    return(rval)
}

#' @export
print.test <- function(x) {

    cat(x$text, ": ", x$pval, "\n", sep="")

    cat("          --------------------\n")

    pad <- "Test: "
    for(text in x$description) {
        cat(pad); pad <- "      "
        cat(text)
        cat("\n")
    }
}
