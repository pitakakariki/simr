
powerPlot <- function(x, n, col=lcrblue, bg=lighten(col), add=FALSE, join=TRUE, xname='x') {
    
    # Confidence intervals
    z <- binom.confint(x, n, 0.95, "logit")

    # Plot
    plotx <- seq(3, length=length(x))
    ploty <- x/n
    
    plotCI(plotx, ploty, ylim=c(0,1), ui=z$upper, li=z$lower,
        xlab=str_c("levels of ", xname), ylab="power",
        yaxt="n",
        col=col, pch= 21, add=add, cex.lab=1)

    axisy <- seq(0, 1, 0.2)
    axis(2, at=axisy, lab=str_c(pretty(axisy) * 100, '%'), las=TRUE, cex.lab=2)

    abline(h=0)
    abline(h=1)
    
    # Decoration
    if(join) lines(plotx, ploty, col=alpha(col, 0.75), lwd=2, lty=4)
    points(plotx, ploty, col=col, bg=bg, pch=21)
    
}


#' @S3method plot powerCurve
#'
plot.powerCurve <- function(z, pval=z$pval, ...) {
    
    pal <- getPalette(length(pval))
    
    for(i in seq_along(pval)) {
        
        x <- sapply(z$pa, function(x) sum(x$pval < pval[[i]]))
        n <- sapply(z$pa, "[[", "n")
        
        powerPlot(x, n, add=(i!=1), xname=z$xname, col=pal[[i]], ...)
    }
    
    if(length(pval) > 1) legend('topleft', col=pal, pt.bg=lighten(pal), pch=21, legend=pval, bg='white')
}