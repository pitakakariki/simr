pvalMCMC <- function(

    fit,
    nSim = 1000,
    
    xname = all.vars(as.formula(fit))[2],

    seed = NA,
    
    ...
    
    ) {
    
    # Null model.
    fixef(fit)[xname] <- 0
    
    f <- function() {
        
        # simulate
        y <- doSim(fit) 
        
        # fit
        a <- doFit(y, fit, ...)
                
        # get fixed effect
        
        ##TODO## nice way of getting the right fixed effect
        
        fixef(a)[xname]
    }

    dist <- raply(nSim, f(), .progress=progress_simr("MC Test Setup"))
    
    rval <- function(fit) {
    
        x <- fixef(fit)[xname]
        
        ##TODO## get tails right?
        rank <- sum(abs(dist) > abs(x))
        
        return((rank + 1) / (nSim + 1))
    }
}