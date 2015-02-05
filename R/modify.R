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
#' fm <- lmer(y ~ x + (1|g), data=example)
#' fixef(fm)
#' fixef(fm)["x"] <- -0.1
#' fixef(fm)
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
calcTheta <- function(V, sigma=attr(V, "sc")) {

    theta <- llply(V, calcTheta1, sigma)

    unname(unlist(theta))
}

#' @rdname modify
#' @export
`VarCorr<-` <- function(object, value) {

    sigma <- attr(value, "sc")
    if(is.null(sigma)) sigma <- sigma(object)

    object@theta <- calcTheta(value)

    return(object)
}

#' @rdname modify
#' @export
`sigma<-` <- function(object, value) {

    useSc <- object@devcomp$dims[["useSc"]]
    REML <- object@devcomp$dims[["REML"]]

    if(!useSc) stop("sigma is not applicable for this model.")

    V <- VarCorr(object)

    sigmaName <- if(REML) "sigmaREML" else "sigmaML"
    object@devcomp$cmp[[sigmaName]] <- value
    object@theta <- calcTheta(V, value)

    return(object)
}

#' @rdname modify
#' @export
`scale<-` <- function(object, value) {

    useSc <- object@devcomp$dims[["useSc"]]
    REML <- object@devcomp$dims[["REML"]]

    if(!useSc) stop("scale is not applicable for this model.")

    sigmaName <- if(REML) "sigmaREML" else "sigmaML"
    object@devcomp$cmp[[sigmaName]] <- value

    return(object)
}