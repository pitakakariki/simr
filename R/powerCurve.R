#' Calculate the power for an analysis at a range of levels.
#'
#' @export
#'
#'
#'
#'
#'
#'
#'
#'
#'

powerCurve <- function(
    
    fit,
    
    nSim = .SIMRDEFAULT_NSIM,
    
    xname = getDefaultXname(fit),
    
    sim = fit,

    pval = 0.05,
    
    seed = 23
    
    ) {
    
    if(!is.na(seed)) set.seed(seed)
    
    this.frame <- getFrame(fit)
    
    ##TODO## specify which subsets we cover
    
    # auto subsetting
    ss_list <- with(this.frame, {

        .x <- get(xname)
        
        target <- tail(sort(unique(.x)), -2)

        lapply(target, function(up) .x <= up)
    
    })

    msg <- str_c("Calculating power at ", length(ss_list), " sample sizes for ", xname)
    message(msg)

    simulations <- llply(1:nSim, function(.) doSim(sim), .progress=progress_simr("Simulating"))
    
    z <- list(
        pa = llply(ss_list, function(ss) powerSim(fit=fit, nSim=nSim, sim=iter(simulations), subset=ss), .progress=counter_simr()),
        pval = pval,
        xname = xname
    )
    
    rval <- structure(z, class="powerCurve")
    
    .SIMRLASTRESULT <<- rval
    
    return(rval)
}

#' @export
print.powerCurve <- function(x) {
  
  lapply(x$pa, print)
}




