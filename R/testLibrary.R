#
# Extra info for printing
#

wrapTest <- function(test, text="[user defined]", description="[user defined function]") {

    if(is.null(attr(test, "text"))) attr(test, "text") <- text
    if(is.null(attr(test, "description"))) attr(test, "description") <- description

    return(test)
}


#
# Lots of possible tests...
#

#
# Return a stub for wrapTest
#
#' @export
fixed <- function(xname, method=c("lr", "z", "kr", "pb"), ...) {

    method <- match.arg(method)

    test <- switch(method,
        lr = lrtest,
        z  = ztest,
        kr = krtest,
        pb = pbtest
    )

    description <- switch(method,
        lr = "Likelihood ratio",
        z  = "Z-test",
        kr = "Kenward Roger (package pbkrtest)",
        pb = "Parametric bootstrap (package pbkrtest)"
    )

    rval <- function(.) test(., xname)

    wrapTest(rval, str_c("for predictor '", xname, "'"), description)
}

#
# Wrapper for anova
#
#' @export
compare <- function(model, method="lr", ...) {

    rval <- function(fit1) {

        fit2 <- update(fit1, formula(model), evaluate=FALSE)
        fit2 <- eval(fit2, env=environment(formula(fit1)))

        suppressMessages(anova(fit1, fit2, test="Chisq")$Pr[2]) # supress ML refit messages
    }

    description <- "Likelihood ratio"
    description[2] <- str_c("Comparison to ", deparse(formula(model)))

    wrapTest(rval, "to compare models", description)
}


#
# Return a test function for random effects
#
#' @export
random <- function() {

    rval <- function(.) exactRLRT(.)$p.value
    rval <- wrapTest(rval, "for a single random effect", "Exact restricted LRT (package RLRsim)")

    return(rval)
}

#
# simplest test - just grab the p-value from the model's summary.
# nb: This will be a z-test (Wald) for glmerMod objects
#     t-test for lm/glm?

ztest <- function(fit, xname) {

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
# test using drop1 --- use this to build krtest and pbtest
#
drop1test <- function(fit, xname, fun, ...) {

    # formula for dropped variable
    dropname <- as.formula(c("~", xname))

    a <- drop1(fit, dropname, test="user", sumFun=fun, ...)
    rval <- a[xname, "p.value"]

    return(rval)
}

# from ?drop1.merMod in lme4
krWrap <- function(object, objectDrop, ...) {

    krnames <- c("ndf", "ddf", "Fstat", "p.value", "F.scaling")

    if(missing(objectDrop)) return(setNames(rep(NA, length(krnames)), krnames))

    krtest <- KRmodcomp(object, objectDrop)
    rval <- unlist(krtest$stats[krnames])

    return(rval)
}

krtest <- function(fit, xname) drop1test(fit, xname, krWrap)

pbWrap <- function(object, objectDrop, ...) {

    pbnames <- c("stat", "df", "p.value")

    if(missing(objectDrop)) return(setNames(rep(NA, length(pbnames)), pbnames))

    pbtest <- PBmodcomp(object, objectDrop, nsim=getSimrOption("pbnsim"))
    rval <- unlist(pbtest$test["PBtest", pbnames])

    return(rval)
}

pbtest <- function(fit, xname) drop1test(fit, xname, pbWrap)

lrWrap <- function(object, objectDrop, ...) {




}