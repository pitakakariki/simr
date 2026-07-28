context("Binomial")

## binary response

zbin <- with(simdata, z < 3)
glm_bin1 <- glm(zbin ~ x + g, family="binomial", data=simdata)
glmm_bin1 <- glmer(zbin ~ x + (1|g), family="binomial", data=simdata)

test_that("binomial with binary response works", {

    y <- doSim(glm_bin1)
    expect_true(all(y * (1-y) == 0))

    temp <- doTest(doFit(doSim(glm_bin1), glm_bin1))


    y <- doSim(glmm_bin1)
    expect_true(all(y * (1-y) == 0))

    temp <- doTest(doFit(doSim(glmm_bin1), glmm_bin1))

})

## cbind response

# note the weird number of trials - z+10, z successes and 10 failures

glm_bin2 <- glm(cbind(z, 10) ~ x + g, family="binomial", data=simdata)
glmm_bin2 <- glmer(cbind(z, 10) ~ x + (1|g), family="binomial", data=simdata)

test_that("binomial with cbind response works", {

    y <- doSim(glm_bin2)
    expect_identical(dim(y), c(30L, 2L))

    temp <- doTest(doFit(doSim(glm_bin2), glm_bin2))

    y <- doSim(glmm_bin2)
    expect_identical(dim(y), c(30L, 2L))

    temp <- doTest(doFit(doSim(glmm_bin2), glmm_bin2))

})

## proportion response

zweight <- rep(10, nrow(simdata))
zprop <- with(simdata, z/zweight)

glm_bin3 <- glm(zprop ~ x + g, family="binomial", data=simdata, weights=zweight)
glmm_bin3 <- glmer(zprop ~ x + (1|g), family="binomial", data=simdata, weights=zweight)

zweight_b <- zweight + c(0,1)
zprop_b <- with(simdata, z/zweight_b)

glmm_bin3b <- glmer(zprop_b ~ x + (1|g), family="binomial", data=simdata, weights=zweight_b)

test_that("binomial with proportion response works", {

    y <- doSim(glm_bin3)
    expect_equal(zweight*y, round(zweight*y))
    expect_true(!all(y %in% c(0, 1)))

    temp <- doTest(doFit(doSim(glm_bin2), glm_bin2))

    y <- doSim(glmm_bin3)
    expect_equal(zweight*y, round(zweight*y))
    expect_true(!all(y %in% c(0, 1)))

    temp <- doTest(doFit(doSim(glmm_bin3), glmm_bin3))

    expect_warning(xm_bin3 <- extend(glmm_bin3b, along="g", n=5), "not supported")

})

## Mixing Poisson and binomial

## Various binomial specifications
test_that("binonial with cbind(s,f) response works", {

    simdata_bin <- simdata
    simdata_bin$n <- simdata$z + simdata$x

    glmm_bin4 <- glmer(cbind(z, x) ~ (1|g), family=binomial, data=simdata_bin)
    glmm_bin5 <- glmer(cbind(z, n-z) ~ (1|g), family=binomial, data=simdata_bin)

    set.seed(456); btest4 <- doFit(doSim(glmm_bin4), glmm_bin4)
    set.seed(456); btest5 <- doFit(doSim(glmm_bin5), glmm_bin5)

    expect_equal(rowSums(doSim(glmm_bin4)), with(simdata, z+x))
    expect_equal(fixef(btest4), fixef(btest5))
})
