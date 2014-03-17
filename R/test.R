getDefaultTest <- function(x, ...) UseMethod('getDefaultTest')

getDefaultTest.default <- function(x, ...) tTest(x, ...)

getDefaultTest.merMod <- function(x, ...) pvalMCMC(x, ...)





tTest <- function(x, ...) function(fit) summary(fit)$coefficients["x", "Pr(>|t|)"]