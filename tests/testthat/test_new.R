context("From scratch")

x <- rep(1:10)
g <- c('a', 'b', 'c')

X <- expand.grid(x=x, g=g)
X2 <- rbind(X, X)

b <- c(2, -0.1) # fixed intercept and slope
V1 <- 0.5 # random intercept variance
V2 <- matrix(c(0.5,0.05,0.05,0.1), 2) # random intercept and slope variance-covariance matrix
s <- 1 # residual variance

# for cbind binomial
bdata <- simdata
bdata$s <- bdata$z
bdata$f <- rev(bdata$z^2)
bdata$n <- bdata$s + bdata$f

bbeta <- c(-1, 0.1)
btheta <- 1

test_that("makeLmer works", {

    model1 <- makeLmer(y ~ x + (1|g), fixef=b, VarCorr=V1, sigma=s, data=X)

    expect_equivalent(fixef(model1), b)
    expect_equivalent(sigma(model1), s)

    expect_equal(c(VarCorr(model1)$g), V1)

    ps <- powerSim(model1, nsim=1)
    expect_equal(nrow(ps$warnings), 0)
    expect_equal(nrow(ps$errors), 0)
})

test_that("makeLmer works consistently", {

    model1a <- makeLmer(y ~ x + (1|g), fixef=b, VarCorr=V1, sigma=2, data=X2)
    model1b <- makeLmer(y ~ x + (1|g), fixef=b, VarCorr=V1, sigma=2, data=X2)

    expect_equal(VarCorr(model1a), VarCorr(model1b))
    expect_equal(getME(model1a, "theta"), getME(model1b, "theta"))
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

test_that("makeGlmer works for cbind-binomial", {

    bm1 <- makeGlmer(cbind(s, n-s) ~ x + (1 | g), family=binomial, data=bdata, fixef=bbeta, VarCorr=btheta)
    bm2 <- makeGlmer(cbind(s, f) ~ x + (1 | g), family = binomial, data=bdata, fixef=bbeta, VarCorr=btheta)
    # need to reset weights for bm3-6
    bm3 <- makeGlmer(cbind(1+s, 1+f) ~ x + (1 | g), family = binomial, data=bdata, fixef=bbeta, VarCorr=btheta)
    bm4 <- makeGlmer(cbind(s+1, f+1) ~ x + (1 | g), family = binomial, data=bdata, fixef=bbeta, VarCorr=btheta)
    bm5 <- makeGlmer(cbind(3, f) ~ x + (1 | g), family = binomial, data=bdata, fixef=bbeta, VarCorr=btheta)
    bm6 <- makeGlmer(cbind(s, 3) ~ x + (1 | g), family = binomial, data=bdata, fixef=bbeta, VarCorr=btheta)

    bsim1 <- doSim(bm1)
    expect_equal(ncol(bsim1), 2)
    expect_equal(rowSums(bsim1), bdata$n)
})
