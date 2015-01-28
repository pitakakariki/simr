#
# Lots of possible tests...
#

#
# simplest test - just grab the p-value from the model's summary.
# nb: This will be a z-test (Wald) for glmerMod objects
#     t-test for lm/glm?

simpletest <- function(fit, xname) {

    a <- summary(fit)$coefficients
    testname <- grep("Pr\\(", colnames(a), value=TRUE)
    rval <- a[xname, testname]

    return(rval)
}

#
# basic likelihood ratio test using drop1
#

lrtest <- function(fit, xname) {

    dropname <- as.formula(c("~", xname))

    a <- drop1(fit, dropname, test="Chisq")
    rval <- a[xname, "Pr(Chi)"]

    return(rval)
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