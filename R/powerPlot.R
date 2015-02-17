
powerPlot <- function(z, x, n, col=lcrblue, bg=lighten(col), add=FALSE, join=TRUE) {

    # Confidence intervals
    ci <- binom.confint(x, n, 0.95, "logit")

    # Plot
    plotx <- z $ nlevels
    ploty <- x/n

    plotCI(plotx, ploty, ylim=c(0,1), ui=ci$upper, li=ci$lower,
        xlab=str_c("number of levels of ", z$along),
        ylab="power",
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


#' @export
#'
plot.powerCurve <- function(z, pval=z$pval, power=0.80, ...) {

    pal <- getPalette(length(pval))

    for(i in seq_along(pval)) {

        x <- sapply(z$ps, function(x) sum(x$pval < pval[[i]], na.rm=TRUE))
        n <- sapply(z$ps, "[[", "n")

        powerPlot(z, x, n, add=(i!=1), col=pal[[i]], ...)
    }

    if(is.numeric(power)) abline(h=power, lty=2)
    if(length(pval) > 1) legend('topleft', col=pal, pt.bg=lighten(pal), pch=21, legend=pval, bg='white')
}