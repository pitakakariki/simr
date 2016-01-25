context("Logging")

test_that("warnings and errors are logged by powerSim", {

    simf <- function() sometimes(doSim(fm1), p=0.2, pw=0.3, lambda=5)

    ps <- powerSim(fm1, sim=simf, nsim=10, seed=12345)

    expect_equal(ps$warnings$index, c(rep(4,7),6,10,10))
    expect_equal(ps$errors$index, c(3,10))
})
