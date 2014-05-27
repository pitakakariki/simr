getDefaultTest <- function(x, ...) UseMethod('getDefaultTest')

getDefaultTest.default <- function(x, ...) tTest(x, ...)

#getDefaultTest.merMod <- function(x, ...) pvalMCMC(x, ...)
getDefaultTest.merMod <- function(...) chisqTest




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

chisqTest <- function(model, xname=getDefaultXname(model), ...) {
  
  dropname <- as.formula(c("~", xname))
  a <- drop1(model, dropname, test="Chisq")
  
  a[xname, "Pr(Chi)"]
}