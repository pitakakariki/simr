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

#
# Create a working `sumFun` from a comparison function
#
drop1wrap <- function(f, method="abc") {

    function(a, b, ...) {



        f(a, b)

        attr(rval, "method") <- method
    }
}