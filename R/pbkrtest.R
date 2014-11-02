krSumFun <- function(a, b, ...) {
    
    rval <- if(missing(b)) {
        
        c(pval=NA)
        
    } else {
        
        c(pval=KRmodcomp(a,b)$stats$p.value)
    }
    
    attr(rval, "method") <- "KR (pbkrtest)"
    
    return(rval)
}