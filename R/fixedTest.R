#
# Return a stub for wrapTest
#
fixed <- function(xname, method=c("lr", "z", "kr", "pb"), ...) {

    method <- match.arg(method)

    test <- switch(method,
        lr = lrtest,
        z  = ztest,
        kr = krtest,
        pb = pbtest
    )

    rval <- function(.) test(., xname)

    return(rval)
}

compare <- function(model, method="", ...) {

    rval <- function(fit1) {

        fit2 <- update(fit1, formula(model), evaluate=FALSE)
        fit2 <- eval(fit2, env=environment(formula(fit1)))

        anova(fit1, fit2, refit=FALSE)$Pr[2]
    }

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




