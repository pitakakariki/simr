context('extend')

test_that('extended model has correct dimensions', {

    x1 <- extend(fm1, along="x", n=20)

    expect_equal(nrow(getData(x1)), 2*nrow(getData(fm1)))


    x2 <- extend(fm1, along="g", n=15)

    expect_equal(nrow(getData(x2)), 5*nrow(getData(fm1)))


    x3 <- extend(fm1, within="x+g", n=3)

    expect_equal(nrow(getData(x3)), 3*nrow(getData(fm1)))


    x4 <- extend(flm, along="x", n=20)

    expect_equal(nrow(getData(x4)), 2*nrow(getData(flm)))
})

##
## Add a lot more tests
##

# works for data.frame, lm, glm, lmer, glmer.

# n, values, addn, addvalues

