context("Test Library")

test_that("tests run", {

    t1 <- doTest(fm1)
    expect_equal(c(t1), 0.0007336556, tolerance=1e-5)
    expect_output(t1, "Kenward Roger \\(package pbkrtest\\)")

    t2 <- doTest(fm1, fixed("x", "lr"))
    expect_equal(c(t2), 0.0005147106, tolerance=1e-5)
    expect_output(t2, "Likelihood ratio")

    t3 <- doTest(fm1, random())
    expect_equal(c(t3), 0, tolerance=1e-5)
    expect_output(t3, "Exact restricted LRT \\(package RLRsim\\)")

    t4 <- doTest(fm1, compare(~ (1|g)))
    expect_equal(c(t4), 0.0005147106, tolerance=1e-5)
    expect_output(t4, "Likelihood ratio")

    t5 <- doTest(fm1, fcompare(~ 1))
    expect_equal(c(t5), 0.0005147106, tolerance=1e-5)
    expect_output(t5, "Likelihood ratio")

    t6 <- doTest(fm3, rcompare(~ (1|g)))
    expect_equal(c(t6), 0.6029132, tolerance=1e-5)
    expect_output(t6, "Likelihood ratio")

    t7 <- suppressWarnings(doTest(fm3, rcompare(~ (1|g), "pb")))
    expect_equal(c(t7), 0.1666667, tolerance=1e-5)
    expect_output(t7, "Parametric bootstrap \\(package pbkrtest\\)")


})
