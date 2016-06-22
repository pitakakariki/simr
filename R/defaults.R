getDefaultXname <- function(obj) {

    rhs <- formula(obj)[[3]]

    a <- all.vars(rhs)[[1]]
    b <- str_trim(str_split(deparse(rhs), stringr::fixed("+"))[[1]][1])

    if(a != b) stop("Couldn't automatically determine a default fixed effect for this model.")

    return(a)
}

