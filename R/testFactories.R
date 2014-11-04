#
# Return a test function for fixed effects
#
fixed <- function(xname, test) {








}

#
# Build a test factory using drop1
#
drop1test <- function() {

    # formula for dropped variable

    # specify comparison test

    # use drop1
}


chisqTest <- function(model, xname=getDefaultXname(model), ...) {

  dropname <- as.formula(c("~", xname))
  a <- drop1(model, dropname, test="Chisq")

  a[xname, "Pr(Chi)"]
}