getDefaultTest <- function(x, ...) UseMethod('getDefaultTest')

getDefaultTest.default <- function(x, ...) tTest(x, ...)

getDefaultTest.merMod <- function(x, ...) pvalMCMC(x, ...)





tTest <- function(
  fit,
  xname = getDefaultXname(fit),
  testname = grep("Pr\\(", colnames(summary(fit)$coefficients), value=TRUE),
  ...
  ) {

  # return this test function
  function(x) {
    
    summary(x)$coefficients[xname, testname]
  }
}