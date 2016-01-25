context("Test Library")

test_that("tests run", {

    expect_equal(c(doTest(fm1)), 0.0007336556, tolerance=1e-5)

    expect_equal(c(doTest(fm1, random())), 0, tolerance=1e-5)

    expect_equal(c(doTest(fm1, compare(~ (1|g)))), 0.0005147106, tolerance=1e-5)

    expect_equal(c(doTest(fm1, fcompare(~ 1))), 0.0005147106, tolerance=1e-5)

    expect_equal(c(doTest(fm3, rcompare(~ (1|g)))), 0.6029132, tolerance=1e-5)

})
