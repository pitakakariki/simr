

#gm1.y <- with(cbpp, cbind(incidence, size - incidence))
#gm1 <- glmer(gm1.y ~ period + (1 | herd), data = cbpp, family = binomial)



#p <- with(example, plogis(y / 4 - 3))
#set.seed(123)
#b <- rbinom(length(p), 1, p)


#' @export
doFit.glmerMod <- function(y, fit, subset, ...) {

    # need to have tests
    #stopifnot(is(model, "merModLmerTest"))

    newData <- getData(fit)
    responseName <- as.character(as.formula(fit)[[2]])

    # hack for binomial
    if(responseName[1] == "cbind") {

        responseName <- responseName[2]
        y <- y[, responseName]
    }

    newData[[responseName]] <- y

    newData <- newData[subset, ]

    newCall <- fit@call
    newCall[["data"]] <- newData
    if("control" %in% names(newCall)) newCall[["control"]] <- NULL
    newCall[[1]] <- quote(glmer)

    #if(getSimrOption("lmerhint")) newCall[["start"]] <- getME(model, "theta")

    rval <- eval(newCall)

    ##TODO## do this properly. maybe an lme4 bugfix
    #environment(attr(rval@frame, "formula")) <- as.environment(newData)

    return(rval)
}
