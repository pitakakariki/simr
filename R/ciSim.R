
#' @export
ci <- function(xname, method="profile") {

    xname <- ifelse(xname=="1", "(Intercept)", xname)

    rval <- function(fit, alpha=0.05) {

        CI <- confint(fit, level=1-alpha, method=method, quiet=TRUE)

        return(CI[xname, , drop=FALSE])
    }

    wrapTest(rval, "CIs", "Profile confidence intervals")
}

#' @export
ciWidth <- function(xname, method="profile") {

    xname <- ifelse(xname=="1", "(Intercept)", xname)

    rval <- function(fit, alpha=0.05) {

        CI <- confint(fit, level=1-alpha, method=method, quiet=TRUE)

        CI <- CI[xname, , drop=FALSE]

        return(CI[2] - CI[1])
    }

    wrapTest(rval, "CIs", "Profile confidence intervals")
}

# test = ciWidth(getDefaultXname(fit))
#
# should become ??? = ci???(???(fit))

#' @export
getWidths <- function(x) UseMethod("getWidths")

#' @export
getWidths.powerSim <- function(x) {

    z <- x$pval

    c(
        mean = mean(z, na.rm=TRUE),
        sd = sd(z, na.rm=TRUE),
        lq = as.vector(quantile(z, 0.025, na.rm=TRUE)),
        uq = as.vector(quantile(z, 0.975, na.rm=TRUE))
    )
}

#' @export
getWidths.powerCurve <- function(x) {

    z <- lapply(x$ps, getWidths)

    rval <- do.call(rbind, z)

    rownames(rval) <- x$xval

    return(rval)
}

#' @export
ciSim <- function(

    fit,
    test = ci(getDefaultXname(fit)),
    sim = fit,

    nsim = getSimrOption("nsim"),
    alpha = 0.05,

    seed,

    ...

    ) {

    # setup
    if(!missing(seed)) set.seed(seed)
    #this.frame <- getFrame(fit)

    #test(fit) # throw any errors now

    # generate the simulations
    simulations <- maybe_rlply(nsim, doSim(sim), .text="Simulating")

    # fit the model to the simualtions
    z <- maybe_llply(simulations, doFit, fit, .text="Fitting", ...)

    # summarise the fitted models
    test <- wrapTest(test)
    ci <- maybe_llply(z, test, alpha=alpha, .text="Calculating CIs")

#.ci <<- ci

    # UGLY FIX THIS
    ciArray <- list_to_matrix(ci$value)
    dimnames(ciArray)[[3]] <- seq_len(nsim)

    ### coverage

    upper <- slice2(ciArray, 2)
    lower <- slice2(ciArray, 1)

    #coverage <- (lower <= fixef(sim)) & (fixef(sim) <= upper) ## fixef only for lme4 merMod objects!!!

    ### width

    width <- upper - lower

    # structure the return value
    rval <- list()

    rval $ ci <- ciArray
    rval $ upper <- upper
    rval $ lower <- lower

    rval $ width <- width ### FIX NAMES

    #rval $ xname <- xname
    #rval $ effect <- fixef(sim)[xname] # can't guarantee this is available?

    rval $ text <- attr(test, "text")(fit, sim)
    rval $ description <- attr(test, "description")(fit, sim)

    rval $ warnings <- ci$warnings
    rval $ errors <- ci$errors

    class(rval) <- "ciSim"

    .simrLastResult $ lastResult <- rval

    return(rval)
}

#' @export
print.ciSim <- function(x, ...) {

    cat("Mean CI widths: ")
    cat(mean(x $ width, na.rm=TRUE)) # replace w/ CI
    cat("\n\n")

    pad <- "CI Type: "
    for(text in x$description) {
        cat(text)
        cat("\n")
    }
    cat("\n")

    #cat(sprintf("Based on %i simulations and effect size %.2f", z$n, z$effect))
    cat(sprintf("Based on %i simulations, ", x$n))
    wn <- length(unique(x$warnings$index)) ; en <- length(unique(x$errors$index))
    wstr <- str_c(wn, " ", if(wn==1) "warning" else "warnings")
    estr <- str_c(en, " ", if(en==1) "error" else "errors")
    cat(str_c("(", wstr, ", ", estr, ")"))
    cat("\n")
}

#' @export
plot.ciSim <- function(x, ...) stop("Not yet implemented.")

#' @export
summary.ciSim <- function(object, level=0.95, pval=object$pval, method=getSimrOption("binom"), ...) {

    x <- object$x
    n <- object$n

    power <- binom.confint(x, n, level, method)[c("mean", "lower", "upper")]

    rval <- cbind(successes=x, trials=n, power)

    class(rval) <- c("summary.ciSim", class(rval))

    return(rval)
}

### simplification helpers

list_to_matrix <- function(x) {

# must be all the same length, with maybe some zeroes
    d <- unique(llply(x, dim_))
    if(0 %in% d) d <- d[-match(0, d)]
    if(length(d) != 1) stop("multiple dimensionalities found")
    d <- d[[1]]

    # nb NULL -> NA
    x <- ifelse(laply(x, is.null), list(array(NA, d)), x)

    # they should probably be atomic too
    if(any(laply(x, is.recursive))) stop("recursive elements found")

    simplify2array(x)
}

dim_ <- function(x) if(is.null(dim(x))) length(x) else dim(x)

ndim_ <- function(x) length(dim_(x))

drop_ <- function(x, n) {

    # check that n'th dimension has length one.
    if(length(dim_(x)[n]) != 1) stop("can't drop a dimension if length isn't one")

    dn <- dimnames(x)[-n]
    dim(x) <- dim(x)[-n]
    dimnames(x) <- dn

    return(x)
}

slice2 <- function(x, i) {

    drop_(x[i,,, drop=FALSE], 1)
}

slice2 <- function(x, i) {

    drop_(x[,i,, drop=FALSE], 2)
}

slice3 <- function(x, i) {

    drop_(x[,,i, drop=FALSE], 3)
}

