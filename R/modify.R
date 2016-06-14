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

    fixefNames <- colnames(getME(object, 'X'))
    nameTest <- setdiff(names(value), fixefNames)

    if(length(nameTest) != 0) {

        stop(str_c(nameTest[[1]], " is not the name of a fixed effect."))
    }

    object @ beta <- unname(value)

    simrTag(object) <- TRUE

    return(object)
}

#' @rdname modify
#' @export
`coef<-` <- function(object, value) UseMethod("coef<-", object)

#' @export
`coef<-.default` <- function(object, value) {

    object $ coefficients <- value
    object $ fitted.values <- predict(object, type="response")

    simrTag(object) <- TRUE

    return(object)
}

#' @export
`coef<-.glm` <- function(object, value) {

    object $ coefficients <- value
    object $ linear.predictors <- predict.lm(object, type="response")
    object $ fitted.values <- family(object)$linkinv(object $ linear.predictors)

    simrTag(object) <- TRUE

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

    object@theta <- calcTheta(value, s)

    simrTag(object) <- TRUE

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

    simrTag(object) <- TRUE

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

    simrTag(object) <- TRUE

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

    simrTag(object) <- TRUE

    return(object)
}

# Unmodified objects suggest post hoc power analysis.

simrTag <- function(object) {

    isTRUE(attr(object, "simrTag"))
}

`simrTag<-` <- function(object, value) {

    attr(object, "simrTag") <- value

    return(object)
}

observedPowerWarning <- function(sim) {

    if(simrTag(sim)) return(FALSE)

    if(is.function(sim)) return(FALSE)

    if(is(sim, "iter")) return(FALSE)

    if(!getSimrOption("observedPowerWarning")) return(FALSE)

    warning("This appears to be an \"observed power\" calculation")

    return(TRUE)
}
