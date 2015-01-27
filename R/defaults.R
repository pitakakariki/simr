getDefaultXname0 <- function(x) {

    n <- names(fixef(x))

    if(n[1] == "(Intercept)") {

        if(is.na(n[2])) {

            stop("Couldn't determine a default fixed effect.")

        } else {

            n[2] # i.e. first name after the intercept
        }

    } else n[1]
}

getDefaultXname <- function(obj) {

    obj.formula <- as.formula(obj)
    obj.rhs <- obj.formula[[3]]
    obj.x <- all.vars(obj.rhs)

    obj.x[[1]]
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
