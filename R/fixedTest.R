#
# Return a stub for wrapTest
#
fixed <- function(xname, method="", ...) {

    rval <- list(type="fixed", xname=xname, method=method)
    class(rval) <- "simrTestDef"

    return(rval)
}

#
# Turn a stub into a test
#
wrapTest <- function(obj, fit) {

    if(inherits(obj, "simrTestDef")) {

        if(obj$type == "fixed") {

            xname <- obj$xname
            x <- getData(fit)[[xname]]

            test.f <- function(fit) lrtest(fit, xname)

        } else {

            stop(str_c("Test type ", obj$type, " not implemented."))
        }


    } else if(inherits(obj, tail(class(fit), 1))) {


        stop("NYI")

    } else if(inherits(obj, "function")) {


        stop("NYI")

    } else {

        stop(str_c("Tests not implemented for objects of class ", class(obj)[[1]]))
    }

    rval <- test.f

    return(rval)
}




