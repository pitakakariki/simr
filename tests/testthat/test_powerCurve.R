context("powerCurve")

test_that("", {

    pc1 <- powerCurve(fm1, nsim=3)

    expect_equal(nrow(pc1$warnings), 0)
    expect_equal(nrow(pc1$errors), 0)

    pc2 <- powerCurve(fm1, along="g", nsim=3)

    expect_equal(nrow(pc2$warnings), 0)
    expect_equal(nrow(pc2$errors), 0)

    pc3 <- powerCurve(fm1, within="x+g", nsim=3)

    expect_equal(nrow(pc3$warnings), 0)
    expect_equal(nrow(pc3$errors), 0)

})
