#
# Tests should be quick and clean.
#

test.nsim <- 10
test.progress <- FALSE

helperopts <- simrOptions(nsim=test.nsim, progress=test.progress)

#
# Useful to have an example model.
#

fm1 <- lmer(y ~ x + (1|g), data=example)
