getDefaultTest <- function(x, ...) UseMethod('getDefaultTest')

getDefaultTest.default <- function(x, ...) tTest(x, ...)

#getDefaultTest.merMod <- function(x, ...) pvalMCMC(x, ...)
getDefaultTest.merMod <- function(...) chisqTest
getDefaultTest.glmerMod <- function(fit, xname, ...) ttest(xname)

# Random Effects tests:
#require(RLRsim)
#powerCurve(a, test=function(.) exactRLRT(.)$p.value, nSim=250)

