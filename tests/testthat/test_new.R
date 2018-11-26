context("From scratch")

x <- rep(1:10)
g <- c('a', 'b', 'c')

X <- expand.grid(x=x, g=g)

b <- c(2, -0.1) # fixed intercept and slope
V1 <- 0.5 # random intercept variance
V2 <- matrix(c(0.5,0.05,0.05,0.1), 2) # random intercept and slope variance-covariance matrix
s <- 1 # residual variance

test_that("makeLmer works", {

    model1 <- makeLmer(y ~ x + (1|g), fixef=b, VarCorr=V1, sigma=s, data=X)

    expect_equivalent(fixef(model1), b)
    expect_equivalent(sigma(model1), s)

    expect_equal(c(VarCorr(model1)$g), V1)

    ps <- powerSim(model1, nsim=1)
    expect_equal(nrow(ps$warnings), 0)
    expect_equal(nrow(ps$errors), 0)
})

test_that("makeGlmer works", {

    model2 <- makeGlmer(z ~ x + (x|g), family="poisson", fixef=b, VarCorr=V2, data=X)

    expect_equivalent(fixef(model2), b)

    expect_equal(c(VarCorr(model2)$g), c(V2))

    ps <- powerSim(model2, nsim=1)
    expect_equal(nrow(ps$warnings), 0)
    expect_equal(nrow(ps$errors), 0)

    rval <- X
    model3 <- makeGlmer(z ~ x + (x|g), family="poisson", fixef=b, VarCorr=V2, data=rval)

    expect_error(getData(model3), NA)

    model4 <- makeGlmer(cbind(z, 12) ~ x + (x|g), family="poisson", fixef=b, VarCorr=V2, data=X)

    expect_error(getData(model4), NA)

})
