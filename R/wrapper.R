###
##
## Functions for lme4 interoperability.
##
###

##
## Rebuild a merMod object
##

#
# A 'copied' object will differ in:
#
# @call
# @optinfo
#

#' 'Fit' a mixed model by returning previously calculated parameters.
#'
#' Dummy 'optimizer' that allows `lmer` to be called without re-estimating parameters.
#'
#' @export
#'
#'
#'
nullOpt <- function(fn, par, lower, upper, control) {

    rval <- list(
        fval        = 0,
        par         = control$theta,
        convergence = 0,
        message     = "Fit using supplied values.",
        control     = list()
    )

    # calling the deviance function updates its environment
    rval$fval <- fn(rval$par)

    return(rval)
}

merCopy <- function(object) UseMethod("merCopy")

merCopy.lmerMod <- function(object) {

    # theta from merMod object
    theta=unname(getME(object, "theta"))

    lmerControl(optimizer="nullOpt", restart_edge=FALSE, optCtrl=list(theta=theta))
}

merCopy.glmerMod <- function(object) {

    # theta from merMod object
    theta=unname(getME(object, "theta"))

    glmerControl(optimizer="nullOpt", restart_edge=FALSE, optCtrl=list(theta=theta))
}
