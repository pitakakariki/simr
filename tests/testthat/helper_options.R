#
# Tests should be quick and clean.
#

test.nSim <- 10
test.progress <- FALSE

helperopts <- simrOptions(nSim=test.nSim, progress=test.progress)

#
# Useful to have an example model.
#

fm1 <- lmer(y ~ x + (1|g), data=example)
