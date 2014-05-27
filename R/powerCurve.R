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
    along = xname,
    
    sim = fit,

    pval = 0.05,
    
    seed = 23
    
    ) {

    # START TIMING
    timing <- system.time({
  
    if(!is.na(seed)) set.seed(seed)
    
    this.frame <- getFrame(fit)
    
    ##TODO## specify which subsets we cover
    
    # auto subsetting
    ss_list <- with(this.frame, {

        x <- get(along)
        
        target <- tail(sort(unique(x)), -2)

        lapply(target, function(up) x <= up)
    
    })

    msg <- str_c("Calculating power at ", length(ss_list), " sample sizes for ", along)
    message(msg)

    simulations <- llply(1:nSim, function(.) doSim(sim), .progress=progress_simr("Simulating"))
    
    z <- list(
        pa = llply(ss_list, function(ss) powerSim(fit=fit, xname=xname, nSim=nSim, sim=iter(simulations), subset=ss), .progress=counter_simr()),
        pval = pval,
        xname = xname,
        along = along
    )
    
    rval <- structure(z, class="powerCurve")
    
    .SIMRLASTRESULT <<- rval
    
    })
    # END TIMING
    
    rval $ timing <- timing
    
    return(rval)
}

#' @export
print.powerCurve <- function(x) {
  
  lapply(x$pa, print)
  
  cat("Time elapsed: ")
  cat(x$timing['elapsed'])
  cat("s\n")
}

timed <- function(f, mode=c("attribute", "list")) {
  
  mode <- match.arg(mode)
  
  function(...) {
    
    timing <- system.time(rval <- eval.parent(substitute(f(...))), gc=TRUE)
    
    if(mode == "list") rval$timing <- timing
    if(mode == "attribute") attr(rval, "timing") <- timing
    
    return(rval)
  }
}

