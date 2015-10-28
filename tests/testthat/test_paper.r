context("Paper examples")

# Make sure this is replicable.
set.seed(42)

test_that("Tutorials work.", {

  ps1 <- powerSim(fm1)

  expect_is(ps1, "powerSim")
  expect_equal(ps1$x, 9)
  expect_equal(ps1$n, 10)

  expect_equal(ps1$pval, c(0.00116776738713373, 0.0295454542836253, 1.21560063674469e-05,
    0.000498979507405491, 5.92823358089596e-05, 0.00132992922350937,
    4.59434991028126e-06, 0.000667631113289252, 0.187538595459622,
    2.1720438196426e-05), tolerance=1e-7)
})


# pc1 <- powerCurve(model1)
# plot(pc1)
#
#
# fixef(model1)
# fixef(model1)["x"]
#
#
# model2 <- model1
# fixef(model2)["x"] <- -0.1
#
#
#
# pc2 <- powerCurve(model2)
# plot(pc2)
#
#
#
# model3 <- extend(model2, along="x", n=20)
# pc3 <- powerCurve(model3)
# plot(pc3)
#
#
#
# plot(pc3, pval=c(0.01, 0.05, 0.10))
#
#
# model4 <- extend(model2, along="g", n=15)
# pc4 <- powerCurve(model4, along="g")
# plot(pc4, pval=c(0.01, 0.05, 0.10))

