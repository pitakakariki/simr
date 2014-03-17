
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
#'

powerCurve <- function(
    
    fit,
    
    R = .SIMRDEFAULT_R,
    
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

    simulations <- llply(1:R, function(.) doSim(sim), .progress=progress_simr("Simulating"))
    
    z <- list(
        pa = llply(ss_list, function(ss) power(fit=fit, R=R, sim=iter(simulations), subset=ss), .progress=counter_simr()),
        pval = pval,
        xname = xname
    )
    
    structure(z, class="powerCurve")
}






