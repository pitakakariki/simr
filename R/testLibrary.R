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
#' The \code{method} argument can be used to specify one of the following tests.
#' Note that \code{"z"} is an asymptotic approxiimation for models not fitted
#' with \code{\link[lme4]{glmer}} and \code{"kr"} will only work with models
#' fitted with \code{\link[lme4]{lmer}}.
#'
#' \describe{
#' \item{\code{z}:}{
#'     Z-test for models fitted with \code{\link[lme4]{glmer}} (or \code{\link{glm}}),
#'     using the p-value from \code{\link[=summary.merMod]{summary}}.
#'     For models fitted with \code{\link[lme4]{lmer}}, this test can be used to
#'     treat the t-values from \code{\link[=summary.merMod]{summary}} as
#'     z-values, which is equivalent to assuming infinite degrees of freedom.
#'     This asymptotic approximation seems to perform well for even medium-sized
#'     data sets, as the denominator degrees of freedom are already quite large
#'     (cf. Baayen et al. 2008) even if calculating their exact value is
#'     analytically unsolved and computationaly difficult (e.g. with
#'     Satterthwaite or Kenward-Roger approximations). Setting
#'     \code{alpha=0.045} is roughly equal to the t=2 threshold suggested by
#'     Baayen et al. (2008) and helps compensate for the slightly
#'     anti-conservative approximation.}
#' \item{\code{t}:}{
#'     T-test for models fitted with \code{\link{lm}}. Also available for mixed models
#'     when \code{\link[lmerTest]{lmerTest}} is installed, using the p-value calculated
#'     using the Satterthwaite approximation for the denominator degrees of
#'     freedom by default. This can be changed by setting \code{lmerTestDdf},
#'     see \code{\link{simrOptions}}.}
#' \item{\code{lr}:}{Likelihood ratio test, using \code{\link[=anova.merMod]{anova}}.}
#' \item{\code{f}:}{
#'      Wald F-test, using \code{\link[=Anova]{car::Anova}}.
#'      Useful for examining categorical terms. For to models fitted with
#'      \code{\link[lme4]{lmer}}, this should yield equivalent results to
#'      \code{method='kr'}. Uses Type-II tests by default, this can be changed
#'      by setting \code{carTestType}, see \code{\link{simrOptions}}.}
#' \item{\code{chisq}:}{
#'      Wald Chi-Square test, using \code{\link[=Anova]{car::Anova}}.
#'      Please note that while this is much faster than the F-test computed with
#'      Kenward-Roger, it is also known to be anti-conservative, especially for
#'      small samples. Uses Type-II tests by default, this can be changed by
#'      setting \code{carTestType}, see \code{\link{simrOptions}}.}
#' \item{\code{anova}:}{
#'      ANOVA-style F-test, using \code{\link{anova}} and
#'      \code{\link[lmerTest:anova.merModLmerTest]{lmerTest::anova}}. For `lm`, this
#'      yields a Type-I (sequential) test (see \code{\link[=anova.lm]{anova}});
#'      to use other test types, use the F-tests provided by \code{car::Anova()}
#'      (see above). For \code{lmer}, this generates Type-II tests with
#'      Satterthwaite denominator degrees of freedom by default, this can be
#'      changed by setting \code{lmerTestDdf} and \code{lmerTestType}, see
#'      \code{\link{simrOptions}}.}
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
#' @references
#' Baayen, R. H., Davidson, D. J., and Bates, D. M. (2008). Mixed-effects modeling
#' with crossed random effects for subjects and items. Journal of Memory and Language, 59, 390--412.
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
fixed <- function(xname, method=c("z", "t", "f", "chisq", "anova", "lr", "kr", "pb")) {

    method <- if(missing(method)) "default" else match.arg(method)

    test <- switch(method,
        default = defaulttest,
        z  = ztest,
        t  = ttest,
        f = waldftest,
        lr = lrtest,
        chisq = waldchisqtest,
        anova = anovatest,
        kr = krtest,
        pb = pbtest
    )

    descriptionText <- switch(method,
        default = "default",
        z  = "z-test",
        t  = "t-test",
        f = paste0("Type-",getSimrOption("carTestType"), " F-test (package car)"),
        lr = "Likelihood ratio",
        chisq = paste0("Type-",getSimrOption("carTestType"), " Chi-Square-test (package car)"),
        anova = "F-test",
        kr = "Kenward Roger (package pbkrtest)",
        pb = "Parametric bootstrap (package pbkrtest)"
    )

    description <- fixeddesc(descriptionText, xname)

    rval <- function(.) test(., xname)

    wrapTest(rval, str_c("for predictor '", removeSquiggle(xname), "'"), description)
}

fixeddesc <- function(text, xname) {

    function(fit, sim) {

        if(text %in% c("t-test") & inherits(fit,"merMod")){
            text <- paste(text,"with",getSimrOption("lmerTestDdf"),
                          "degrees of freedom (package lmerTest)")
        }else  if(text %in% c("F-test")){
            if(inherits(fit,"merMod")){
                text <- paste0("Type-", getSimrOption("lmerTestType")," ", text,
                               " with ",getSimrOption("lmerTestDdf")," degrees of freedom (package lmerTest)")
            }else{
                text <-  paste("Type-I",text)
            }
        }

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

    x <- getData(fit)[[xname]]

    if(is.factor(x) || is.character(x)) {

        return(lrtest(fit, xname))
    }

    switch(class(fit)[1],

        lm       = ttest(fit, xname),
        glm      = ztest(fit, xname),
        lmerMod  = krtest(fit, xname),
        glmerMod = ztest(fit, xname),
        stop(str_c("No default test for ", class(fit)[1]))
    )
}

checkInteractions <- function(fit, xname) {

    ts <- terms(fit)

    order <- attr(ts, "order")
    label <- attr(ts, "term.labels")

    xname %in% unlist(str_split(label[order > 1], stringr::fixed(":")))
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

    if("merModLmerTest" %in% class(fit)){
        # block costly ddf calculations for lmerTest fits since we're using
        # the asymptotic approximation anyway
        a <- summary(fit,ddf="lme4")$coefficients
    }else{
        a <- summary(fit)$coefficients
    }

    if(inherits(fit,"lmerMod")){
        # multiple by 2 for two-tailed test (which is what we want on coefs)
        rval <- pnorm(a[xname, "t value"],lower.tail=FALSE)*2
    }else{
        rval <- a[xname, "Pr(>|z|)"]
    }

    return(rval)
}

ttest <- function(fit, xname) {

    xname <- removeSquiggle(xname)

    if(inherits(fit,"merMod")){
      if(inherits(fit,"merModLmerTest")){
        # we assume that lmerTest is present, if we have an object of class lmerTest
        # no typecast necessary here
        a <- lmerTest::summary(fit)$coefficients
      }else{
        if(requireNamespace("lmerTest",quietly = TRUE)){
          warning(paste("Using",getSimrOption("lmerTestDdf"),"approximation from lmerTest (casting merMod to merModLmerTest)"))
          fit <- as(fit,"merModLmerTest")
          a <- lmerTest::summary(fit,ddf=getSimrOption("lmerTestDdf"))$coefficients
        }else{
          stop("t-tests for lmer-fitted models require the lmerTest package")
        }
      }
    }else{
      a <- summary(fit)$coefficients
    }

    rval <- a[xname, "Pr(>|t|)"]
    return(rval)
}

#
# Wald tests for linear hypotheses using car::Anova()
#
waldftest <- function(fit,xname){
  if(checkInteractions(fit, xname)) warning("Main effect (", xname, ") was tested but there were interactions.")

  xname <- removeSquiggle(xname)

  if(inherits(fit,"merMod") & !isREML(fit)){
    warning("F test available only for linear mixed model fit by REML: refitting model with REML.")
    fit <- update(fit,REML=TRUE)
  }

  a <- Anova(fit,test.statistic="F",type=getSimrOption("carTestType"))
  rval <- a[xname, "Pr(>F)"]

  return(rval)
}

waldchisqtest <- function(fit,xname){
  if(checkInteractions(fit, xname)) warning("Main effect (", xname, ") was tested but there were interactions.")

  xname <- removeSquiggle(xname)

  a <- Anova(fit,test.statistic="Chisq",type=getSimrOption("carTestType"))
  rval <- a[xname, "Pr(>Chisq)"]

  return(rval)
}

#
# F-tests using anova() and lmerTest::anova()
#

anovatest <- function(fit,xname){
    if(checkInteractions(fit, xname)) warning("Main effect (", xname, ") was tested but there were interactions.")

    xname <- removeSquiggle(xname)

    if(inherits(fit,"merMod")){
        if(inherits(fit,"merModLmerTest")){
            # we assume that lmerTest is present, if we have an object of class lmerTest
            # no typecast necessary here
            a <- lmerTest::anova(fit,ddf=getSimrOption("lmerTestDdf"),type=getSimrOption("lmerTestType"))
        }else{
            if(requireNamespace("lmerTest",quietly = TRUE)){
                warning(paste("Using",getSimrOption("lmerTestDdf"),"approximation from lmerTest (casting merMod to merModLmerTest)"))
                fit <- as(fit,"merModLmerTest")
                a <- lmerTest::anova(fit,ddf=getSimrOption("lmerTestDdf"),type=getSimrOption("lmerTestType"))
            }else{
                stop("anova-tests for lmer-fitted models require the lmerTest package")
            }
        }
    }else{
        a <- anova(fit)
    }

    rval <- a[xname, "Pr(>F)"]

    return(rval)
}

#
# basic likelihood ratio test using drop1
#

lrtest <- function(fit, xname) {

    if(checkInteractions(fit, xname)) warning("Main effect (", xname, ") was tested but there were interactions.")

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

krtest <- function(fit, xname) {
  if(checkInteractions(fit, xname)) warning("Main effect (", xname, ") was tested but there were interactions.")

  drop1test(fit, xname, krWrap)
}

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