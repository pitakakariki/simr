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


test_that("Parallel powerSim with shared memory doParallel works", {
    # On *nix, registerDoParallel works its magic via forking and shared memory
    # while on Windows this silently creates a(n implicit) PSOCK cluster, but
    # this behavior is poorly documented: Unfortunately, this cluster isn't
    # registered and there doesn't seem to be a way to extract it, so it's not
    # possible to set the random seed and thus not possible to test, so we skip
    # this test on Windows
    skip_on_os("windows")

    if(!require(doParallel)){
        skip("doParallel not available")
    }
    # set the number of cores -- failing to do so
    # generates an automatic warning message that
    # is upgraded to an error in TravisCI
    # the warning seems to appear only for cores > 2, see parallel:::.check_ncores
    registerDoParallel(cores = 2)

    # setting the same seed for each worker means that we get duplicate answers,
    # which doesn't make sense for real analysis, but is fine for testing
    ps1p <- powerSim(fm1,seed=42,parallel=TRUE,paropts=list(set.seed=42),progress=FALSE)
    # setting the parallel seed is subtle and complicated -- it differs by
    # parallel backend AND the main system seed interacts with the seeding of
    # the parallel workers

    expect_is(ps1p, "powerSim")
    expect_equal(ps1p$x, 10)
    expect_equal(ps1p$n, 10)

    expect_equal(ps1p$pval, rep(0.00116776738713373, 10), tolerance=1e-7)

    expect_equal(confint(ps1p), structure(c(0.691502892181239, 1), .Dim = 1:2, .Dimnames = list(
        "power", c("2.5 %", "97.5 %"))))
})

test_that("Parallel powerSim with distributed memory doParallel works", {
    if(!require(doParallel)){
        skip("doParallel not available")
    }
    cl <- makePSOCKcluster(2)
    registerDoParallel(cl)

    clusterSetRNGStream(cl, 42)
    ps1p <- powerSim(fm1,parallel=TRUE,progress=FALSE)
    # setting the parallel seed is subtle and complicated -- it differs by
    # parallel backend AND the main system seed interacts with the seeding of
    # the parallel workers

    expect_is(ps1p, "powerSim")
    expect_equal(ps1p$x, 10)
    expect_equal(ps1p$n, 10)

    # these values have to be sorted because they're not guaranteed to be
    # returned in any particular order -- such is the parallel world with
    # external scheduling

    expect_equal(sort(ps1p$pval), c(1.11823467246945e-05, 3.1866056529441e-05, 0.000277024778880028,
      0.000418968149031732, 0.00104467772091231, 0.00109926926386851,
      0.00157018740372611, 0.00393018236344348, 0.00863423688823151,
      0.011228752019506), tolerance=1e-7)

    expect_equal(confint(ps1p), structure(c(0.691502892181239, 1), .Dim = 1:2, .Dimnames = list(
        "power", c("2.5 %", "97.5 %"))))
    stopCluster(cl)
})
