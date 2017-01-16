alpha <- function(x, alpha=1) {

    alpha0 <- col2rgb(x, alpha=TRUE)[4]

    rgb(t(col2rgb(x)), alpha=alpha0*alpha, maxColorValue=255)
}

lighten <- Vectorize(function(col) {

    rgb <- col2rgb(col)

    # lighten
    f <- 150
    rgb <- f + (1 - f/255) * rgb

    r <- rgb[[1]]
    g <- rgb[[2]]
    b <- rgb[[3]]

    rgb(r, g, b, max=255)
})

ci_abline <- function(
    fit,
    col = 'palevioletred',
    alpha = 0.5,
    npts = 1000) {

    xrange <- range(fit$model$x)
    x <- seq(xrange[1], xrange[2], length=npts)

    pred <- predict(fit, newdata=data.frame(x=x), interval='confidence')

    polygon(c(x, rev(x)), c(pred[,2], rev(pred[,3])), col=alpha(col, alpha), border=NA)
    lines(x, pred[,2], col=col, lwd=2)
    lines(x, pred[,3], col=col, lwd=2)

    lines(x, pred[,1], col=col, lwd=3)
}

lcrgreen <- '#639441'
lcrlightgreen <- '#EBF1E5'
lcrblue <- '#008FC5'
lcrlightblue <- '#E2E9F3'
lcrbrown <- '#767662'
lcrlightbrown <- '#EFF0EB'

plotpal <- function(n=length(x), x=getPalette(n)) {

    plot(seq_along(x), rep(1, n), col=x, bg=lighten(x), pch=21, cex=10, xlim=c(0, n+1), lwd=3)

    invisible(x)
}

getPalette <- function(n) {

    start <- c(lcrblue, lcrgreen, lcrbrown)

    if(n <= 3) return(start[seq_len(n)])

    return(c(start, seq_len(n-3)))
}
