#' Specify a statistical test to apply
#'
#' @name tests
#' @rdname tests
#'
#' @param xname an explanatory variable to test (character).
#' @param model a null model for comparison (formula).
#' @param method the type of test to apply (see Details).
#'
#' @details
#'
#' \describe{
#' \item{\code{fixed}:}{
#'     Test a single fixed effect, specified by \code{xname}.}
#' \item{\code{compare}:}{
#'     Compare the current model to a smaller one specified by the formula \code{model}.}
#' \item{\code{fcompare}, \code{rcompare}:}{
#'     Similar to \code{compare}, but only the fixed/random part of the formula needs to be supplied.}
#' \item{\code{random}:}{
#'     Test the significance of a single random effect.}
#' }
#'
#' @section Methods:
#'
#' The \code{method} argument can be used to specify one of the following tests. Note that \code{"z"}
#' is only applicable to models fitted in \code{\link[lme4]{glmer}} and \code{"kr"} will only work with models
#' fitted with \code{\link[lme4]{lmer}}.
#'
#' \describe{
#' \item{\code{z}:}{
#'     Z-test for models fitted with \code{\link[lme4]{glmer}} (or \code{\link{glm}}),
#'     using the p-value from \code{\link[=summary.merMod]{summary}}.}
#' \item{\code{t}:}{T-test for models fitted with \code{\link{lm}}}
#' \item{\code{lr}:}{Likelihood ratio test, using \code{\link[=anova.merMod]{anova}}.}
#' \item{\code{kr}:}{
#'     Kenward-Roger test, using \code{\link[pbkrtest]{KRmodcomp}}.
#'     This only applies to models fitted with \code{\link[lme4]{lmer}}, and compares models with
#'     different fixed effect specifications but equivalent random effects.}
#' \item{\code{pb}:}{
#'     Parametric bootstrap test, using \code{\link[pbkrtest]{PBmodcomp}}.
#'     This test will be very accurate, but is also very computationally expensive.}
#' }
#'
#' Tests using \code{random} for a single random effect call \code{\link[RLRsim]{exactRLRT}}.
#'
#' @return
#'
#' A function which takes a fitted model as an argument and returns a single p-value.
#'
#' @examples
#' lm1 <- lmer(y ~ x + (x|g), data=simdata)
#' lm0 <- lmer(y ~ x + (1|g), data=simdata)
#' anova(lm1, lm0)
#' compare(. ~ x + (1|g))(lm1)
#' rcompare(~ (1|g))(lm1)
#' \dontrun{powerSim(fm1, compare(. ~ x + (1|g)))}
#'
NULL

## ----------
##
## User-visible test definition functions. These are suitable for test= arguments.
##
## ----------

#
# Test a fixed effect
#
#' @rdname tests
#' @export
fixed <- function(xname, method=c("z", "t", "lr", "kr", "pb")) {

    method <- if(missing(method)) "default" else match.arg(method)

    test <- switch(method,
        default = defaulttest,
        z  = ztest,
        t  = ttest,
        lr = lrtest,
        kr = krtest,
        pb = pbtest
    )

    descriptionText <- switch(method,
        default = "default",
        z  = "z-test",
        t  = "t-test",
        lr = "Likelihood ratio",
        kr = "Kenward Roger (package pbkrtest)",
        pb = "Parametric bootstrap (package pbkrtest)"
    )

    description <- fixeddesc(descriptionText, xname)

    rval <- function(.) test(., xname)

    wrapTest(rval, str_c("for predictor '", removeSquiggle(xname), "'"), description)
}

fixeddesc <- function(text, xname) {

    function(fit, sim) {

        # test used
        rval <- if(text=="default") defaultdesc(fit, xname) else text

        # effect size
        fe <- maybe(fixef)(sim)$value
        if(!is.null(fe) && xname %in% names(fe)) {

            rval[2] <- sprintf("Effect size for %s is %#.2g", xname, fe[[xname]])
        }

        return(rval)
    }
}

# default fixed effect test
# lm - ttest
# glm - ztest
# lmer - krtest
# glmer - ztest
defaulttest <- function(fit, xname) {

    if(is.factor(getData(fit)[[xname]])) return(lrtest(fit, xname))

    switch(class(fit)[1],

        lm       = ttest(fit, xname),
        glm      = ztest(fit, xname),
        lmerMod  = krtest(fit, xname),
        glmerMod = ztest(fit, xname),
        stop(str_c("No default test for ", class(fit)[1]))
    )
}

defaultdesc <- function(fit, xname) {

    if(is.factor(getData(fit)[[xname]])) return("Likelihood ratio")

    switch(class(fit)[1],

        lm       = "t-test",
        glm      = "z-test",
        lmerMod  = "Kenward Roger (package pbkrtest)",
        glmerMod = "z-test",
        "unknown test"
    )
}

#
# Compare two models
#
#' @rdname tests
#' @export
compare <- function(model, method=c("lr", "pb")) {

    method <- match.arg(method)

    test <- switch(method,
        lr = lrcompare,
        pb = pbcompare
    )

    description <- switch(method,
        lr = "Likelihood ratio",
        pb = "Parametric bootstrap (package pbkrtest)"
    )

    rval <- function(fit1) {

        fit2 <- update(fit1, formula(model), evaluate=FALSE)
        fit2 <- eval(fit2, envir=environment(formula(fit1)))

        test(fit1, fit2)
    }

    description[2] <- str_c("Comparison to ", deparse(formula(model)))

    wrapTest(rval, "for model comparison", description)
}


#' @rdname tests
#' @export
fcompare <- function(model, method=c("lr", "kr", "pb")) {

    method <- match.arg(method)

    test <- switch(method,
        lr = lrcompare,
        kr = krcompare,
        pb = pbcompare
    )

    description <- switch(method,
        lr = "Likelihood ratio",
        kr = "Kenward-Roger (package pbkrtest)",
        pb = "Parametric bootstrap (package pbkrtest)"
    )

    rval <- function(fit1) {

        fe.part <- deparse(nobars(formula(model)))
        re.part <- do.call(str_c, c(llply(findbars(formula(fit1)), function(.) str_c("(", deparse(.), ")")), sep=" + "))

        new.formula <- str_c(fe.part, " + ", re.part)

        fit2 <- update(fit1, as.formula(new.formula), evaluate=FALSE)
        fit2 <- eval(fit2, envir=environment(formula(fit1)))

        test(fit1, fit2)
    }

    description[2] <- str_c("Comparison to ", deparse(formula(model)), " + [re]")

    wrapTest(rval, "for model comparison", description)
}

#' @rdname tests
#' @export
rcompare <- function(model, method=c("lr", "pb")) {

    method <- match.arg(method)

    test <- switch(method,
        lr = lrcompare,
        pb = pbcompare
    )

    description <- switch(method,
        lr = "Likelihood ratio",
        pb = "Parametric bootstrap (package pbkrtest)"
    )

    rval <- function(fit1) {

        fe.part <- deparse(nobars(formula(fit1)))
        re.part <- laply(findbars(formula(model)), function(.) str_c("(", deparse(.), ")"))

        new.formula <- str_c(fe.part, " + ", re.part)

        fit2 <- update(fit1, as.formula(new.formula), evaluate=FALSE)
        fit2 <- eval(fit2, envir=environment(formula(fit1)))

        test(fit1, fit2)
    }

    description[2] <- str_c("Comparison to [fe] + ", deparse(formula(model)))

    wrapTest(rval, "for model comparison", description)
}

#
# Single random effects via RLRsim
#
#' @rdname tests
#' @export
random <- function() {

    rval <- function(.) exactRLRT(.)$p.value
    rval <- wrapTest(rval, "for a single random effect", "Exact restricted LRT (package RLRsim)")

    return(rval)
}

## ----------
##
## Helper functions
##
## ----------

wrapTest <- function(test, text="[user defined]", description="[user defined function]") {

    if(is.character(text)) {

        this.text <- str_c("Power ", text)
        text <- function(...) this.text
    }

    if(is.character(description)) {

        this.description <- description
        description <- function(...) this.description
    }

    if(is.null(attr(test, "text"))) attr(test, "text") <- text
    if(is.null(attr(test, "description"))) attr(test, "description") <- description

    return(test)
}

addSquiggle <- function(x) {

    if(inherits(x, "formula")) return(x)

    if(inherits(x, "character")) {

        return(formula(str_c("~", x)))
    }

    stop(str_c("Can't interpret a fixed effect name with class ", class(x)[[1]]))
}

removeSquiggle <- function(x) {

    if(inherits(x, "character")) return(x)

    if(inherits(x, "formula")) {

        return(deparse(x[[length(x)]]))
    }

    stop(str_c("Can't interpret a fixed effect name with class ", class(x)[[1]]))
}

## ----------
##
## Building blocks for fixed effects tests
##
## ----------

#
# simplest test --- just grab the p-value from the model's summary.
# nb: This will be a z-test (Wald) for glmerMod and glm objects,
#     t-test for lm, not available for lmerMod

ztest <- function(fit, xname) {

    xname <- removeSquiggle(xname)

    a <- summary(fit)$coefficients
    rval <- a[xname, "Pr(>|z|)"]

    return(rval)
}

ttest <- function(fit, xname) {

    xname <- removeSquiggle(xname)

    a <- summary(fit)$coefficients
    rval <- a[xname, "Pr(>|t|)"]

    return(rval)
}

#
# basic likelihood ratio test using drop1
#

lrtest <- function(fit, xname) {

    dropname <- addSquiggle(xname)
    xname <- removeSquiggle(xname)

    test <- if(family(fit)$family == "gaussian") "F" else "Chisq"

    a <- drop1(fit, dropname, test="Chisq")
    testname <- grep("Pr\\(", colnames(a), value=TRUE)
    rval <- a[xname, testname]

    return(rval)
}


#
# test using drop1 --- use this to build krtest and pbtest
#
drop1test <- function(fit, xname, fun, ...) {

    # formula for dropped variable
    dropname <- addSquiggle(xname)
    xname <- removeSquiggle(xname)

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


## ----------
##
## Comparison based tests.
##
## ----------

krcompare <- function(model1, model2) {

    KRmodcomp(model1, model2)$stats$p.value
}

pbcompare <- function(model1, model2) {

    PBmodcomp(model1, model2, nsim=getSimrOption("pbnsim"))$test["PBtest", "p.value"]
}

lrcompare <- function(model1, model2) {

    suppressMessages(anova(model1, model2, test="Chisq")$Pr[2]) # supress ML refit messages
}