#' Modify fixed effect coefs.
#'
#' This function replaces the fixed effect coefficients in a fitted model.
#'
#' @export
#'
#' @param object a linear mixed-effects model (\code{lmerMod}) object.
#' @param value  a new vector of fixed effect coefficients.
#'
#' @details
#'
#' This function would normally be used to change the value of individual fixed effect
#' coefficients, see the example for this usage.
#'
#' @examples
#' fm <- lmer(y ~ x + (1|g), data=example)
#' fixef(fm)
#' fixef(fm)["x"] <- -0.1
#' fixef(fm)
#'
`fixef<-` <- function(object, value) {

    fixefNames <- colnames(getME(object, 'X'))
    nameTest <- setdiff(names(value), fixefNames)

    if(length(nameTest) != 0) {

        stop(str_c(nameTest[[1]], " is not the name of a fixed effect."))
    }

    object @ beta <- unname(value)

    return(object)
}

# @usage fixef(m)[index] <- value



#
# Naive version just changes sigma. Note that this breaks use.u=TRUE
#
#' @export
`sigma<-` <- function(object, value) {

    useSc <- object@devcomp$dims[["useSc"]]
    REML <- object@devcomp$dims[["REML"]]

    if(!useSc) stop("sigma is not applicable to this model.")

    sigmaName <- if(REML) "sigmaREML" else "sigmaML"
    object@devcomp$cmp[[sigmaName]] <- value

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




}



