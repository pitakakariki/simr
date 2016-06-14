context("modify")

test_that("errors are thrown", {

    expect_error(fixef(fm1)["z"] <- 3, " is not the name of a fixed effect.")

    expect_error(sigma(fm2) <- 8, "sigma is not applicable for this model.")
    expect_error(sigma(fm2) <- 1, NA)

    expect_error(scale(fm2) <- 5, "scale is not applicable for this model.")

    expect_error(sigma(fglm) <- 8, "sigma is not applicable for this model.")
    expect_error(sigma(fglm) <- NULL, NA)
})

test_that("scale<- modifies VarCorr", {

    scale(fm1) <- 2

    expect_equal(attr(VarCorr(fm1), "sc"), 2)
})