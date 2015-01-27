#
# Return a test function for fixed effects
#
fixed <- function(model, xname=getDefaultXname(model), method, ...) {

    if(class(model)=="lmerMod") {

        rval <- lrtest

        return(rval)
    }

    if(class(model)=="glmerMod") {

        x <- getData(model)[[xname]]

        if(class(x)=="factor") {

            rval <- lrtest
        } else {

            rval <- simpletest
        }

        return(rval)
    }

    stop(str_c("Not implemented for models with class", class(model)))

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