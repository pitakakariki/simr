#
# Tests should be quick and clean.
#

test.nsim <- 10
test.progress <- FALSE

helperopts <- simrOptions(nsim=test.nsim, progress=test.progress)

#
# Useful to have some example models.
#

fm1 <- lmer(y ~ x + (1|g), data=simdata); fixef(fm1) <- fixef(fm1)
fm2 <- glmer(z ~ x + (1|g), family=poisson, data=simdata); fixef(fm2) <- fixef(fm2)

flm <- lm(y ~ x + g, data=simdata); coef(flm) <- coef(flm)
fglm <- glm(z ~ x + g, family=poisson, data=simdata); coef(fglm) <- coef(fglm)

