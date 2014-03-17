#' Fit to multiple response vectors
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
multiFit <- function(resp, model, along="Year", fn=fixef) {
    
    laply(resp, fitSeries, model=model, along=along, fn=fn, .progress="text")
}


fitOne <- function(resp, model, fn, subset) {
    
    thisFit <- refit(model, resp, subset=subset)
    rval <- fn(thisFit)
        
    return(rval)
}

fitSeries <- function(resp, model, along="Year", fn=fixef, brk=autoBreaks(model, along)) {
    
    ssv <- function(b) subsetVector(getData(model)[[along]], b)
    f <- function(b) fitOne(resp, model, fn, ssv(b))
    rval <- sapply(brk, f)
    
    return(rval)
}

fitReplicates <- function(resp, model, subset, fn=fixef) {
    
    # fit seed
    seed <- fitSeed(resp[[1]], model, subset)
    
    seed @ optinfo <- list(optimizer="nullOpt", control=list(theta=unname(getME(seed, 'theta')))) 
    
    # refit replicates
    rval <- laply(
        resp,
        function(y) fn(refit(object=seed, newresp=y[subset])),
        .progress='text'
    )
    
    return(rval)
}

fitSeed <- function(resp, model, subset) {
    
    original <- attr(model, "original")
    newCall <- original @ call
    respName <- newCall$formula[[2]]
    
    .X <- getData(model)
    .X[[respName]] <- resp
    
    .subset <- subset
    
    newCall$data <- .X
    newCall$subset <- .subset
    
    rval <- eval(newCall)
    
    newCall$data <- as.name(".X")
    newCall$subset <- as.name(".subset")
    
    rval @ call <- newCall
    
    return(rval)
}

autoBreaks <- function(model, along="Year") {
    
    x <- getData(model)[[along]]
    rval <- sort(unique(x))[-1]
    
    return(rval)
}

subsetVector <- function(x, brk, cmp=`<=`) {
    
    cmp(x, brk)
}

test <- function(effect, tail=c("two", "lower", "upper")) {
    
    tail <- match.arg(tail)
    
    function(fit) {
        
        x <- fixef(fit)[effect]
        s <- sqrt(vcov(fit)[effect, effect])
    return(x)
        pval <- pt(x/s, 1)
        
        if(tail == "two") return(pval)
        if(tail == "lower") return(if(sign(x) < 0) pval/2 else 1-pval/2)
        if(tail == "upper") return(if(sign(x) > 0) pval/2 else 1-pval/2)
        
        stop("Invalid value for 'tail'")
    }
}

