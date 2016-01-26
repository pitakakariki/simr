context("getData")

test_that("error thrown", {

    expect_error(getData(1), "Couldn't find object's data.")
})
