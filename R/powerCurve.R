#' Calculate the power for an analysis at a range of levels.
#'
#' This function runs \code{powerSim} for a series of designs with a range of sample sizes.
#'
#' @param fit a linear mixed model object.
#' @param nSim the number of simulations to run.
#' @param xname the name of the explanatory variable to be tested for significance.
#' @param along e name of an explanatory variable. This variable will have its number of levels varied.
#' @param sim an object to simulate from, by default this is the same as \code{fit}.
#' @param pval the significance level for the statistical test. Defaults to 0.05.
#' @param seed specify a random number generator seed, for reproducible results.
#'
#' @export
#'
#' @examples
#' fm <- lmer(y ~ x + (1|g), data=example)
#' pc <- powerCurve(fm, nSim=10)
#' print(pc)
#' \dontrun{
#' plot(pc)
#' }
#'
powerCurve <- function(
    
    fit,
    
    nSim = .simrOptions$nSim,
    
    xname = getDefaultXname(fit),
    along = xname,
    
    sim = fit,

    pval = 0.05,
    
    seed = 23,
  
    ...
    
    ) {

    # START TIMING
    timing <- system.time({
  
    if(!is.na(seed)) set.seed(seed)
    
    this.frame <- getFrame(fit)
    
    ##TODO## specify which subsets we cover
    
    # auto subsetting
    ss_list <- with(this.frame, {

        x <- get(along)
        
        #target <- tail(sort(unique(x)), -2)
        targets <- tail(unique(x), -2)

        #lapply(target, function(up) x <= up)
        lapply(seq_along(targets), function(z) x %in% head(targets, z))
  
    })

    msg <- str_c("Calculating power at ", length(ss_list), " sample sizes for ", along)
    message(msg)

    simulations <- llply(1:nSim, function(.) doSim(sim), .progress=progress_simr("Simulating"))
    
    z <- list(
        pa = llply(ss_list, function(ss) powerSim(fit=fit, xname=xname, nSim=nSim, sim=iter(simulations), subset=ss, ...), .progress=counter_simr()),
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
print.powerCurve <- function(x, ...) {

  cat("\rPower to detect effect of ")
  cat(x$xname)
  cat(", (95% confidence interval):\n")
  
  #l_ply(x$pa, function(x) {printerval(x);cat("\n")})
  cat("#levels for", x$along, "\n")
  for(i in seq_along(x$pa)) {
    
    cat(sprintf("%7i: ", i+2))
    printerval(x$pa[[i]], ...)
    cat("\n")    
  }
  
  time <- x$timing['elapsed']
  cat(sprintf("\nTime elapsed: %i h %i m %i s\n", floor(time/60/60), floor(time/60) %% 60, floor(time) %% 60))
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

