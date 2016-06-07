#
# Tests should be quick and clean.
#

# temporarily here until devtools is fixed

helperopts <- simrOptions(nsim=10, progress=FALSE, pbnsim=5)

#
# NB: test lastResult here b/c this needs to be run before any simulations are done
#

context("Oops")

test_that("lastResult works", {

    expect_error(lastResult(), "No result available to recover.")

    ps <- powerSim(fm1, nsim=1)

    expect_identical(lastResult(), ps)

})
