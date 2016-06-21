context("powerSim")

# Make sure this is replicable.
set.seed(42)

test_that("Simple powerSim works", {

  ps1 <- powerSim(fm1)

  expect_is(ps1, "powerSim")
  expect_equal(ps1$x, 9)
  expect_equal(ps1$n, 10)

  expect_equal(ps1$pval, c(0.00116776738713373, 0.0295454542836253, 1.21560063674469e-05,
    0.000498979507405491, 5.92823358089596e-05, 0.00132992922350937,
    4.59434991028126e-06, 0.000667631113289252, 0.187538595459622,
    2.1720438196426e-05), tolerance=1e-7)

  expect_equal(confint(ps1), structure(c(0.554983882971805, 0.997471421455538), .Dim = 1:2, .Dimnames = list(
    "power", c("2.5 %", "97.5 %"))))
})

test_that("GLMM powerSim works", {

  ps2 <- powerSim(fm2)

  expect_is(ps2, "powerSim")
  expect_equal(ps2$x, 9)
  expect_equal(ps2$n, 10)
})

test_that("nsim=0 doesn't break powerSim", {

    expect_error(ps0 <- powerSim(fm1, nsim=0), NA)
    expect_output(print(ps0), "<NA>")
})

