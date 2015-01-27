#
# Return a test function for fixed effects
#
fixed <- function(xname, method, ...) {

    fit <- parent.frame()$fit

    fixed_(fit, xname, method, ...)
}

fixed_ <- function(fit, xname, method, ...) {

    if(missing(xname)) xname <- getDefaultXname(fit)

    if(class(fit)=="lmerMod") {

        rval <- lrtest

        return(rval)
    }

    if(class(fit)=="glmerMod") {

        x <- getData(fit)[[xname]]

        if(class(x)=="factor") {

            rval <- lrtest
        } else {

            rval <- simpletest
        }

        return(rval)
    }

    stop(str_c("Not implemented for models with class", class(fit)))

}


#
# Build a test factory using drop1
#
drop1test <- function(xname, method, ...) {

    # formula for dropped variable
    dropname <- as.formula(c("~", xname))

    rval <- function(fit) {

        # use drop1
        a <- drop1(fit, dropname, test="user", sumFun=method, ...)
        a[xname, "pval"]
    }

    # add attributes?

    return(rval)
}
