context("Base lm/glm")

set.seed(123)

test_that("simr works with lm", {

  temp <- powerSim(flm)

  expect_is(temp, "powerSim")

  expect_equal(nrow(temp$warnings), 0)
  expect_equal(nrow(temp$errors), 0)
})

test_that("simr can combine lm and lmer", {

  temp <- powerSim(fit=flm, sim=fm1)

  expect_is(temp, "powerSim")

  expect_equal(nrow(temp$warnings), 0)
  expect_equal(nrow(temp$errors), 0)
})

test_that("simr works with glm", {

  temp <- powerSim(fglm)

  expect_is(temp, "powerSim")

  expect_equal(nrow(temp$warnings), 0)
  expect_equal(nrow(temp$errors), 0)
})
