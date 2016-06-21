context("User supplied functions")

test_that("doFit works for a function", {


    f <- function(yy) lmer(yy ~ x + (1|g), data=simdata)

    ps <- powerSim(fit=f, sim=fm1, test=fixed("x", "z"))
    expect_equal(nrow(ps$errors), 0)

    g <- function(yy, subset) lmer(yy ~ x + (1|g), data=simdata, subset=subset)

    pc <- powerCurve(fit=g, sim=fm1, test=fixed("x", "z"), along="x")
    expect_equal(nrow(pc$errors), 0)
})
