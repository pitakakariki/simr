context("powerCurve")

test_that("", {

    pc <- powerCurve(fm1, nsim=3)


    expect_equal(nrow(pc$warnings), 0)
    expect_equal(nrow(pc$errors), 0)

})
