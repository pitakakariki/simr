
powerPlot <- function(z, x, n, col=lcrblue, bg=lighten(col), add=FALSE, join=TRUE, xlab) {

    # Confidence intervals
    ci <- binom.confint(x, n, 0.95, "logit")

    # Plot
    plotx <- z $ xval
    ploty <- x/n

    plotCI(plotx, ploty, ylim=c(0,1), ui=ci$upper, li=ci$lower,
        xlab=xlab,
        ylab="power",
        yaxt="n", yaxs="i",
        col=col, pch=21, add=add, cex.lab=1)

    axisy <- seq(0, 1, 0.2)
    axis(2, at=axisy, labels=str_c(pretty(axisy) * 100, '%'), las=TRUE, cex.lab=2)

    #abline(h=0)
    #abline(h=1)

    # Decoration
    if(join) lines(plotx, ploty, col=alpha(col, 0.75), lwd=2, lty=4)
    points(plotx, ploty, col=col, bg=bg, pch=21, xpd=TRUE)

}


#' @export
plot.powerCurve <- function(x, alpha=x$alpha, power=0.80, xlab=x$xlab, ...) {

    pal <- getPalette(length(alpha))

    for(i in seq_along(alpha)) {

        y <- sapply(x$ps, function(ps) sum(ps$pval < alpha[[i]], na.rm=TRUE))
        n <- sapply(x$ps, "[[", "n")

        powerPlot(x, y, n, add=(i!=1), col=pal[[i]], xlab=xlab, ...)
    }

    if(is.numeric(power)) abline(h=power, lty=2)
    if(length(alpha) > 1) legend('topleft', col=pal, pt.bg=lighten(pal), pch=21, legend=alpha, bg='white')
}

plotSim <- function(

    sim,
    x,
    g,
    nsim=10,
    data=getData(sim),
    col=lcrgreen,
    ylim=c(upper, lower),
    ...) {

    if(missing(x)) x <- getDefaultXname(sim)
    x <- eval(parse(text=nse(x)), data, parent.frame())

    if(!missing(g)) g <- eval(nse(g), data, parent.frame())

    rval <- maybe_rlply(nsim, doSim(sim), .progress="none")
    Y <- rval$value

    upper <- max(sapply(Y, max))
    lower <- min(sapply(Y, min))

    plot(x, Y[[1]], type="n", ylim=ylim, ...)

    a <- max(10/nsim, 1/100)

    for(i in seq_along(Y)) {

        points(x, Y[[i]], pch=19, col=alpha(col, a), bg=NA)
    }

    invisible(rval)
}


