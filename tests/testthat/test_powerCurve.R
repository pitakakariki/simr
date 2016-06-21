context("powerCurve")

test_that("powerCurve works", {

    set.seed(40000)

    pc1 <- powerCurve(fm1, nsim=3)
    pc2 <- powerCurve(fm1, along="g", nsim=3)
    pc3 <- powerCurve(fm1, within="x+g", nsim=3)

    expect_equal(nrow(pc1$warnings), 0)
    expect_equal(nrow(pc2$warnings), 0)
    expect_equal(nrow(pc3$warnings), 0)

    expect_equal(nrow(pc1$errors), 0)
    expect_equal(nrow(pc2$errors), 0)
    expect_equal(nrow(pc3$errors), 0)

    expect_equal(summary(pc1)$successes, c(0,1,1,0,0,2,2,2))
    expect_equal(summary(pc2)$successes, c(3))
    expect_equal(summary(pc3)$successes, c(3))

    expect_output(print(pc1), "Power for predictor 'x'")
    expect_output(print(pc2), "Power for predictor 'x'")
    expect_output(print(pc3), "Power for predictor 'x'")

    ci1 <- confint(pc1)
    expect_equal(dim(ci1), c(8,2))
    expect_equal(colnames(ci1), c("2.5 %", "97.5 %"))

})

test_that("long and short powerCurves work", {

    fmx <- extend(fm1, along="x", n=20)

    pc18 <- powerCurve(fmx, nsim=1, pcmax=Inf)
    expect_equal(length(pc18$ps), 18)

    pc5 <- powerCurve(fmx, nsim=1, pcmax=5)
    expect_equal(length(pc5$ps), 5)
})