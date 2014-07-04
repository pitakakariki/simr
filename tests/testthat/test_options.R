context("Options")

# make sure we return everything to normal after option tests!
original <- simrOptions()

test_that("getting options works", {

  expect_identical(getSimrOption("nSim"), test.nSim) 
  
  expect_is(simrOptions(), "list")
  expect_named(simrOptions())
  
  expect_identical(simrOptions("nSim", "progress"), list(nSim=test.nSim, progress=test.progress))
  expect_identical(simrOptions(c("nSim", "progress")), list(nSim=test.nSim, progress=test.progress))
  expect_identical(simrOptions(list("nSim", "progress")), list(nSim=test.nSim, progress=test.progress))
})

test_that("setting options works", {
  
  newopts <- list(nSim=9, binom="logit")

  expect_that(simrOptions(), not(is_identical_to(newopts)))
  oldopts <- simrOptions(newopts)
  expect_identical(simrOptions(names(newopts)), newopts)
  
  simrOptions(oldopts)

  expect_that(simrOptions(), not(is_identical_to(newopts)))
  oldopts <- do.call(simrOptions, newopts)
  expect_identical(simrOptions(names(newopts)), newopts)
})

test_that("options are applied", {

  simrOptions(nSim=5)
  expect_equal(powerSim(fm1)$n, 5)  
})

test_that("options are restored", {
  
  simrOptions(original)
  expect_identical(simrOptions(), original)
})