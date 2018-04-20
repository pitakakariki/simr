context("Contrasts")

set.seed(76)

simdata_con <- within(simdata, {

    f <- as.factor(sample(letters[1:3], nrow(simdata), TRUE))
    contrasts(f) <- contr.sum(3)
})

fm_con <- lmer(y ~ f + (1|g), data=simdata_con)
fixef(fm_con) <- fixef(fm_con)
xm_con <- extend(fm_con, along="g", n=6)

test_that("Contrasts work", {

    temp <- powerSim(xm_con)

    expect_equal(nrow(temp$errors), 0)
})