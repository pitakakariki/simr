#' Modifying model parameters.
#'
#' These functions can be used to change the size of a model's fixed effects,
#' its random effect variance/covariance matrices, or its residual variance.
#' This gives you more control over simulations from the model.
#'
#' @name modify
#' @rdname modify
#'
#' @param object a fitted model object.
#' @param value  new parameter values.
#'
#' @details
#'
#' New values for \code{VarCorr} are interpreted as variances and covariances, not standard deviations and
#' correlations. New values for \code{sigma} and \code{scale} are interpreted on the standard deviation scale.
#' This means that both \code{VarCorr(object)<-VarCorr(object)} and \code{sigma(object)<-sigma(object)}
#' leave \code{object} unchanged, as you would expect.
#'
#' \code{sigma<-} will only change the residual standard deviation,
#' whereas \code{scale<-} will affect both \code{sigma} and \code{VarCorr}.
#'
#' These function can be used to change the value of individual parameters, such as
#' a single fixed effect coefficient, using standard R subsetting commands.
#'
#' @examples
#' fm <- lmer(y ~ x + (1|g), data=simdata)
#' fixef(fm)
#' fixef(fm)["x"] <- -0.1
#' fixef(fm)
#'
#' @seealso \code{\link{getData}} if you want to modify the model's data.
#'
NULL

#' @rdname modify
#' @export
`fixef<-` <- function(object, value) {

    value <- coefCheck(fixef(object), value, "fixed effect")

    object @ beta <- unname(value)

    attr(object, "simrTag") <- TRUE

    return(object)
}

coefCheck <- function(coef, value, thing="coefficient") {

    nc <- names(coef)
    nv <- names(value)

    if(!is.null(nv)) {

        # if there are names, are they correct?
        if(!setequal(nc, nv)) {

            stop(str_c(setdiff(nv, nc)[[1]], " is not the name of a ", thing, "."))
        }

        # do they need to be reordered?
        value <- value[nc]
    }

    # are there the right number of coefficients?
    if(length(coef) != length(value)) stop(str_c("Incorrect number of ", thing, "s."))

    return(value)
}

#' @rdname modify
#' @export
`coef<-` <- function(object, value) UseMethod("coef<-", object)

#' @export
`coef<-.default` <- function(object, value) {

    value <- coefCheck(coef(object), value)

    object $ coefficients <- value
    object $ fitted.values <- predict(object, type="response")

    attr(object, "simrTag") <- TRUE

    return(object)
}

#' @export
`coef<-.glm` <- function(object, value) {

    value <- coefCheck(coef(object), value)

    object $ coefficients <- value
    object $ linear.predictors <- predict.lm(object, type="response")
    object $ fitted.values <- family(object)$linkinv(object $ linear.predictors)

    attr(object, "simrTag") <- TRUE

    return(object)
}

# VarCorr -> theta for a single group
calcTheta1 <- function(V, sigma=1) {

    L <- suppressWarnings(chol(V, pivot=TRUE))
    p <- order(attr(L, "pivot"))
    L <- t(L[p, p])

    L[lower.tri(L, diag=TRUE)] / sigma
}

# All the thetas
calcTheta <- function(V, sigma) {

    if(missing(sigma)) sigma <- attr(V, "sc")
    if(is.null(sigma)) sigma <- 1

    if(!is.list(V)) V <- list(V)

    theta <- llply(V, calcTheta1, sigma)

    unname(unlist(theta))
}

#' @rdname modify
#' @export
`VarCorr<-` <- function(object, value) {

    object.useSc <- isTRUE(attr(VarCorr(object), "useSc"))
    value.useSc <- isTRUE(attr(value, "useSc"))

    if(object.useSc && value.useSc) s <- sigma(object) <- attr(value, "sc")
    if(object.useSc && !value.useSc) s <- sigma(object)
    if(!object.useSc && value.useSc) s <- attr(value, "sc")
    if(!object.useSc && !value.useSc) s <- 1

    newtheta <- calcTheta(value, s)

    if(length(newtheta) != length(object@theta)) stop("Incorrect number of variance parameters.")

    object@theta <- newtheta

    attr(object, "simrTag") <- TRUE

    return(object)
}

#' @rdname modify
#' @export
`sigma<-` <- function(object, value) UseMethod("sigma<-", object)

#' @export
`sigma<-.merMod` <- function(object, value) {

    useSc <- object@devcomp$dims[["useSc"]]
    REML <- object@devcomp$dims[["REML"]]

    if(!useSc && !identical(value, 1)) stop("sigma is not applicable for this model.")

    V <- VarCorr(object)

    sigmaName <- if(REML) "sigmaREML" else "sigmaML"
    object@devcomp$cmp[[sigmaName]] <- value
    object@theta <- calcTheta(V, value)

    attr(object, "simrTag") <- TRUE

    return(object)
}

#' @export
`sigma<-.lm` <- function(object, value) {

    old.sigma <- sigma(object)
    new.sigma <- value

    if(is.null(old.sigma)) {

        if(is.null(value)) return(object)

        stop("sigma is not applicable for this model.")
    }

    object$residuals <- object$residuals * new.sigma / old.sigma

    attr(object, "simrTag") <- TRUE

    return(object)
}

#' @export
sigma.lm <- function(object, ...) summary(object)$sigma

#' @rdname modify
#' @export
`scale<-` <- function(object, value) {

    useSc <- object@devcomp$dims[["useSc"]]
    REML <- object@devcomp$dims[["REML"]]

    if(!useSc) stop("scale is not applicable for this model.")

    sigmaName <- if(REML) "sigmaREML" else "sigmaML"
    object@devcomp$cmp[[sigmaName]] <- value

    attr(object, "simrTag") <- TRUE

    return(object)
}

# Unmodified objects suggest post hoc power analysis.

simrTag <- function(object) {

    isTRUE(attr(object, "simrTag"))
}

observedPowerWarning <- function(sim) {

    if(simrTag(sim)) return(FALSE)

    if(is.function(sim)) return(FALSE)

    if(is(sim, "iter")) return(FALSE)

    if(!getSimrOption("observedPowerWarning")) return(FALSE)

    warning("This appears to be an \"observed power\" calculation")

    return(TRUE)
}




#' @export
`ranef<-` <- function(object, value) {

    nm <- names(ranef(object))

    if(!identical(sort(nm), sort(names(value)))) stop("Factor names don't match.")

    b <- unlist(value[nm])

    u <- solve(getME(object, "Lambda"), b)

    object@pp$setDelu(u)
    object@u <- u

    return(object)
}

