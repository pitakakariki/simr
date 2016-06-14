context("modify")

test_that("errors are thrown", {

    expect_error(fixef(fm1)["z"] <- 3, " is not the name of a fixed effect.")

    expect_error(sigma(fm2) <- 8, "sigma is not applicable for this model.")
    expect_error(sigma(fm2) <- 1, NA)

    expect_error(sigma(fglm) <- 8, "sigma is not applicable for this model.")
    expect_error(sigma(fglm) <- NULL, NA)
})