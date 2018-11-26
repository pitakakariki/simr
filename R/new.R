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

    lhs <- make.names(deparse(formula[[2]])); formula[[2]] <- as.name(lhs)
    rhs <- formula[-2]

    p <- list(beta=fixef, theta=calcTheta(VarCorr))
    if(!missing(sigma)) p$sigma <- sigma

    if(!(lhs %in% names(data))) {

        suppressMessages(
            y <- simulate(rhs, nsim=1, family=family, newparams=p, newdata=data)[[1]]
        )

        data[[lhs]] <- y
    }

    environment(formula) <- environment() # https://github.com/lme4/lme4/issues/177

    theta <- calcTheta(VarCorr, sigma)

    suppressWarnings(
        if(identical(family, "gaussian")) {

            rval <- lmer(formula, data=data, control=lmerSet(theta))

        } else {

            rval <- glmer(formula, family=family, data=data, control=glmerSet(theta))
            rval@call$family <- rval@resp$family$family
        }
    )

    fixef(rval) <- fixef
    VarCorr(rval) <- VarCorr
    if(!missing(sigma)) sigma(rval) <- sigma

    attr(rval, "newData") <- data
    rval@call$data <- parse(text=dataName)[[1]]

    rval@call$control <- NULL

    attr(rval, "simrTag") <- TRUE

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
#' @param formula a formula describing the model (see \code{\link[lme4]{glmer}}).
#' @param family type of response variable (see \code{\link{family}}).
#' @param fixef vector of fixed effects
#' @param VarCorr variance and covariances for random effects.
#'    If there are multiple random effects, supply their parameters as a list.
#' @param sigma residual standard deviation.
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


#
# We need to make merMod objects but we don't need to fit them because we're supplying the parameters
#
nullOpt <- function(fn, par, lower, upper, control) {

    theta <- control$theta
    if(is.null(theta)) theta <- rep(1, length(par))

    rval <- list(
        fval        = fn(theta),
        par         = theta,
        convergence = 0,
        message     = "No optimisation",
        control     = list()
    )

    # calling the deviance function updates its environment
    rval$fval <- fn(rval$par)

    return(rval)
}

lmerSet <- function(theta) lmerControl(

    optimizer=nullOpt,
    optCtrl=list(theta=theta),
    restart_edge=FALSE,
    boundary.tol=0,
    calc.derivs=FALSE
)

glmerSet <- function(theta) glmerControl(

    optimizer=nullOpt,
    optCtrl=list(theta=theta),
    restart_edge=FALSE,
    boundary.tol=0,
    calc.derivs=FALSE
)

# logic from stats::glm
as.family <- function(family) {

    if(is.character(family)) {

        family <- get(family, mode = "function", envir = parent.frame(2))
        family <- family()
    }

    if(is.function(family)) {

        family <- family()
    }

    if(is.null(family$family)) {

        stop("'family' not recognized")
    }

    return(family)
}
