#
# Tests should be quick and clean.
#

# nb: move this to test_aaa.R for now?
helperopts <- simrOptions(nsim=10, progress=FALSE, pbnsim=5)
simrOptions(helperopts) # b/c helpers are not called by load_all

#
# Useful to have some example models.
#

fm1 <- lmer(y ~ x + (1|g), data=simdata, control=lmerControl(optimizer="bobyqa"))
fixef(fm1) <- fixef(fm1)

fm2 <- glmer(z ~ x + (1|g), family=poisson, data=simdata); fixef(fm2) <- fixef(fm2)
fm3 <- glmer(z ~ x + (x|g), family=poisson, data=simdata); fixef(fm3) <- fixef(fm3)

flm <- lm(y ~ x + g, data=simdata); coef(flm) <- coef(flm)
fglm <- glm(z ~ x + g, family=poisson, data=simdata); coef(fglm) <- coef(fglm)

