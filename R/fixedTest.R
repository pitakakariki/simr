#
# Return a test function for fixed effects
#
fixed <- function(xname, method, ...) {








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

lrtest <- function(xname, ...) {

    dropname <- as.formula(c("~", xname))

    rval <- function(fit) {

        a <- drop1(fit, dropname, test="Chisq")
        a[xname, "Pr(Chi)"]
    }

    return(rval)
}

ttest <- function(fit, xname, ...) {

    a <- summary(fit)$coefficients
    testname <- grep("Pr\\(", colnames(a), value=TRUE)
    rval <- a[xname, testname]

    return(rval)
}

ttest0 <- function(xname, ...) {

    rval <- function(fit) {

        a <- summary(fit)$coefficients
        testname <- grep("Pr\\(", colnames(a), value=TRUE)
        a[xname, testname]
    }

    return(rval)
}