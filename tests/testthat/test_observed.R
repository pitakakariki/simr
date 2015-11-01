context("Observed power warning")

test_that("observed power warning is thrown", {

    set.seed(23)

    fm <- lmer(y ~ x + (1|g), data=subset(simdata, x < 8))

    expect_warning(temp <- powerSim(fm), 'This appears to be an "observed power" calculation')

    expect_is(temp, "powerSim")
    expect_equal(temp$x, 8)
    expect_equal(temp$n, 10)

})