context("Paper examples")

stopifnot(require(simr))

# Make sure this runs fast, tidy and replicable.
setSimrNSim(10)
setSimrProgress(FALSE)
set.seed(42)

test_that("Tutorials work.", {

  model1 <- lmer(y ~ x + (1|g), data=example)
  ps1 <- powerSim(model1)
  
  expect_is(ps1, "poweranalysis")
  expect_equal(ps1$x, 9)
  expect_equal(ps1$n, 10)
  
  expect_equal(ps1$pval, c(0.000839839391365946, 0.0251776211937, 6.84552015148529e-06, 
    0.000342931436616207, 3.63467838385362e-05, 0.000963111975407976, 
    2.45574872200404e-06, 0.000466036528599357, 0.175131681318242, 
    1.2618519499251e-05))
  
  expect_output(ps1, "^\nPower to detect effect of x, \\(95% confidence interval\\):\n 90\\.00% \\( 55\\.50,  99\\.75\\)\n\nBased on 10 simulations and effect size -0\\.24$")
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

