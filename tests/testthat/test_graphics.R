#
# To-do: work out how to test graphics in R
#

context("Graphics (NYI)")

test_that("Not yet implemented", {

    ps <- powerSim(fm1, nsim=1)
    expect_error(plot(ps), "Not yet implemented.")
})