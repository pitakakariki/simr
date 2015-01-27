krSumFun <- function(a, b, ...) {

    rval <- if(missing(b)) {

        c(pval=NA)

    } else {

        c(pval=KRmodcomp(a,b)$stats$p.value)
    }

    attr(rval, "method") <- "KR (pbkrtest)"

    return(rval)
}

pbSumFun <- function(a, b, R=100, ...) {

    rval <- if(missing(b)) {

        c(pval=NA)

    } else {

        c(pval=PBmodcomp(a,b,nsim=R)$test["PBtest", "p.value"])
    }

    attr(rval, "method") <- "PB (pbkrtest)"

    return(rval)
}