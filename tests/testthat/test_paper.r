context("Paper examples")

stopifnot(require(simr))

# Make sure this runs fast enough.
setSimrNSim(10)
set.seed(42)

test_that("", {

  model1 <- lmer(y ~ x + (1|g), data=example)
  ps <- powerSim(model1)
  
  expect_is(ps, "poweranalysis")

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

