getDefaultXname <- function(obj) {

    rhs <- formula(obj)[[3]]

    a <- all.vars(rhs)[[1]]
    b <- str_trim(str_split(deparse(rhs), stringr::fixed("+"))[[1]][1])

    if(a != b) stop("Couldn't automatically determine a default fixed effect for this model.")

    return(a)
}

plotpal <- function(n=length(x), x=getPalette(n)) {

    plot(seq_along(x), rep(1, n), col=x, bg=lighten(x), pch=21, cex=10, xlim=c(0, n+1), lwd=3)

    invisible(x)
}

getPalette <- function(n) {

    start <- c(lcrblue, lcrgreen, lcrbrown)

    if(n <= 3) return(start[seq_len(n)])

    return(c(start, seq_len(n-3)))
}
