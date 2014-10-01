context("Base lm")

set.seed(123)

test_that("simr works with lm", {
  
  temp <- maybe(powerSim)(lm(y ~ x + z, data=example))
  
  expect_is(temp$value, "poweranalysis")
  
  expect_equal(length(temp$warnings), 0)
  expect_equal(length(temp$errors), 0)
})

test_that("simr can commbine lm and lmer", {
  
  temp <- maybe(powerSim)(fit=lm(y ~ x + z, data=example), sim=fm1)
  
  expect_is(temp$value, "poweranalysis")
  
  expect_equal(length(temp$warnings), 0)
  expect_equal(length(temp$errors), 0)
})