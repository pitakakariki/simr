
#
# TO-DO
#

#
# document
#
# case where sigma supplied but so is attr(VarCorr, "sc")
#
# check for errors in inputs and give meaningful messages
#  - e.g. sigma missing for lmer
#
# make VarCorr<- more robust, e.g. lenght 1 arguments
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

    return(rval)
}

#' @export
makeLmer <- function(formula, fixef, VarCorr, sigma, data) {

    makeMer(formula, "gaussian", fixef, VarCorr, sigma, data, deparse(substitute(data)))
}

#' @export
makeGlmer <- function(formula, family, fixef, VarCorr, data) {

    makeMer(formula, family, fixef, VarCorr, , data, deparse(substitute(data)))
}
