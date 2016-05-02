
#
# TO-DO
#

#
# document
#
# check for errors in inputs and give meaningful messages
#  - e.g. sigma missing for lmer
#

makeMer <- function(formula, family, fixef, VarCorr, sigma, data, dataName) {

    if(length(formula) < 3) stop("Formula must have left and right hand side")

    lhs <- formula[[2]]
    rhs <- formula[-2]

    p <- list(beta=fixef, theta=calcTheta(VarCorr))
    if(!missing(sigma)) p$sigma <- sigma

    suppressMessages(
        y <- simulate(rhs, nsim=1, family=family, newparams=p, newdata=data)[[1]]
    )

    data[[as.character(lhs)]] <- y

    environment(formula) <- environment() # https://github.com/lme4/lme4/issues/177

    suppressWarnings(
        if(identical(family, "gaussian")) {

            rval <- lmer(formula, data=data)

        } else {

            rval <- glmer(formula, family=family, data=data)
            rval@call$family <- rval@resp$family$family
        }
    )

    fixef(rval) <- fixef
    VarCorr(rval) <- VarCorr
    if(!missing(sigma)) sigma(rval) <- sigma

    attr(rval, "newData") <- data
    rval@call$data <- parse(text=dataName)[[1]]

    simrTag(rval) <- TRUE

    if(dataName=="rval") {

        .rval <- rval
        assign(dataName, data)
        return(.rval)

    } else {

        assign(dataName, data)
        return(rval)
    }
}


#' Create an artificial mixed model object
#'
#' Make a \code{\link[lme4]{merMod}} object with the specified structure and parameters.
#'
#' @param formula a formula descibing the model (see \code{\link[lme4]{glmer}}).
#' @param family type of response variable (see \code{\link{family}}).
#' @param fixef vector of fixed effects
#' @param VarCorr variance and covariances for random effects.
#'    If there are multiple random effects, supply their parameters as a list.
#' @param sigma residual variance.
#' @param data \code{data.frame} of explanatory variables.
#'
#' @export
makeGlmer <- function(formula, family, fixef, VarCorr, data) {

    makeMer(formula, family, fixef, VarCorr, , data, deparse(substitute(data)))
}

#' @rdname makeGlmer
#' @export
makeLmer <- function(formula, fixef, VarCorr, sigma, data) {

    makeMer(formula, "gaussian", fixef, VarCorr, sigma, data, deparse(substitute(data)))
}
