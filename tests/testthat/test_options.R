context("Options")

# make sure we return everything to normal after option tests!
original <- simrOptions()

test_that("getting options works", {

  expect_identical(getSimrOption("nsim"), 10)

  expect_is(simrOptions(), "list")
  expect_named(simrOptions())

  expect_identical(simrOptions("nsim", "progress"), list(nsim=10, progress=FALSE))
  expect_identical(simrOptions(c("nsim", "progress")), list(nsim=10, progress=FALSE))
  expect_identical(simrOptions(list("nsim", "progress")), list(nsim=10, progress=FALSE))
})

test_that("setting options works", {

  newopts <- list(nsim=9, binom="logit")

  expect_that(simrOptions(), not(is_identical_to(newopts)))
  oldopts <- simrOptions(newopts)
  expect_identical(simrOptions(names(newopts)), newopts)

  simrOptions(oldopts)

  expect_that(simrOptions(), not(is_identical_to(newopts)))
  oldopts <- do.call(simrOptions, newopts)
  expect_identical(simrOptions(names(newopts)), newopts)
})

test_that("options are applied", {

  simrOptions(nsim=5)
  expect_equal(powerSim(fm1)$n, 5)
})

test_that("options are restored", {

  simrOptions(original)
  expect_identical(simrOptions(), original)
})