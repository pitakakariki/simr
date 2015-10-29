# simr

Power Analysis for Generalised Linear Mixed Models by Simulation.

### Instructions

A quick example to get you started (full tutorial coming soon):

```
library(simr)
fm1 <- lmer(y ~ x + (1|g), data=simdata)
fixef(fm1)["x"] <- -0.05
powerSim(fm1, nsim=10)
```