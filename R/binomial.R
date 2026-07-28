#
# Binomial models with cbind responses need special treatment
#

# family could be any of:
#  - character family="binomial"
#  - function family=binomial
#  - family family=binomial()
isFamily <- function(family, name) {

    if(is.character(family)) return(family == name)
    if(is.function(family) && "link" %in% formalArgs(family)) return(family()$family == name)
    if("family" %in% class(family)) return(family$family == name)

    stop("Unrecognised family argument")
}

# does the model have a cbind response?
cbindResponse <- function(object) {

    family(object)$family=="binomial" && is.matrix(model.response(model.frame(object)))
}

cbindCheck <- function(formula) {

    if(!is.formula(formula)) formula <- formula(formula)

    # No LHS
    if(length(formula) < 3) return(NULL)

    lhs <- formula[[2]]

    # Not in cbind form
    if(lhs[[1]] != as.symbol("cbind")) return(NULL)

    # cbind(sform, fform)
    sform <- lhs[[2]]
    fform <- lhs[[3]]

    svars <- all.vars(sform)
    fvars <- all.vars(fform)

    ns <- length(svars)
    nf <- length(fvars)

    # Mode A: cbind(s, n-s)
    # sform has one variable s
    # s is also in fform
    if(ns==1 && svars %in% fvars) return(list(mode="Mode A", s=svars, f=character(0)))

    # Mode B: cbind(s, f)
    # single variable in each of sform, fform
    if(ns==1 && nf==1) return(list(mode="Mode B", s=svars, f=fvars))

    # Mode C
    # zero or one variables in each of sform, fform
    if(ns<=1 && nf <=1) return(list(mode="Mode C", s=svars, f=fvars))

    # Otherwise it's an error
    stop("Unrecognised binomial response format")
}

