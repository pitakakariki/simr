
lmerTest_anova <- function(object, ...) {

    # Produce lmerTest-anova table for lmer-model fits (lme4 or lmerTest) with new lmerTest package.
    pkg_version <- "3.0-0"

    if(requireNamespace("lmerTest", quietly=TRUE) && packageVersion("lmerTest") < pkg_version) {

        stop("lmerTest versions < 3.0-0 are not supported.")
    }

    if(requireNamespace("lmerTest", quietly=TRUE) && packageVersion("lmerTest") >= pkg_version) {

        return(anova(lmerTest::as_lmerModLmerTest(object), ...)) # lme4 object
    }

    stop("The lmerTest package is required for this test.")
}

lmerTest_summary <- function(object, ...) {

    # Produce lmerTest-summary for lmer-model fits (lme4 or lmerTest) with new lmerTest package.
    pkg_version <- "3.0-0"

    if(requireNamespace("lmerTest", quietly=TRUE) && packageVersion("lmerTest") < pkg_version) {

        stop("lmerTest versions < 3.0-0 are not supported.")
    }

    if(requireNamespace("lmerTest", quietly=TRUE) && packageVersion("lmerTest") >= pkg_version) {

        return(summary(lmerTest::as_lmerModLmerTest(object), ...)) # lme4 object
    }

    stop("The lmerTest package is required for this test.")
}

is_lmerTest <- function(object) {

    # Check if an object is of class merModLmerTest or lmerModLmerTest
    # Bridges across versions of lmerTest
    inherits(object, "merModLmerTest") || inherits(object, "lmerModLmerTest")
}
