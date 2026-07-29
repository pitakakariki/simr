context("Non-standard response")

simdata_log <- simdata
simdata_log$logy <- log(simdata$y)

test_that("log(y) response works", {

    fm_log <- lmer(log(y) ~ x + (1|g), data=simdata_log)
    fm_nolog <- lmer(logy ~ x + (1|g), data=simdata_log)

    set.seed(18)
    y_log <- doSim(fm_log)
    refit_log <- doFit(y_log, fm_log)

    set.seed(18)
    y_nolog <- doSim(fm_nolog)
    refit_nolog <- doFit(y_nolog, fm_nolog)

    expect_equal(y_log, y_nolog)
    expect_equal(fixef(refit_log), fixef(refit_nolog))
})
