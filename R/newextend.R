rlEncode <- function (x) {

    n <- length(x)
    if (n == 0L)
        return(structure(list(lengths = integer(), values = x),
            class = "rle"))

    y <- tail(x, -1) != head(x, -1)
    i <- c(which(y | is.na(y)), n)

    list(

        lengths = diff(c(0, i)),
        values = x[i]
    )
}

rlDecodeList <- function(valueList, lengths) {

    n <- sum(lengths(valueList) * lengths)
    values <- logical(n)
    indices <- integer(n)

    ix_new <- 0
    ix_old <- 0

    for(i in seq_along(valueList)) {

        for(j in seq_along(valueList[[i]])) {

            for(k in seq_len(lengths[i])) {

                ix_new <- ix_new + 1

                values[ix_new] <- valueList[[i]][[j]]
                indices[ix_new] <- ix_old + k
            }
        }

        ix_old <- ix_old + lengths[i]
    }

    list(values=values, indices=indices)
}

f_ <- function(x) {

    oldValues <- unique(x)
    newValues <- c(oldValues, 42)

    oldValues <- c(oldValues, tail(oldValues, 1))

    rl <- rlEncode(x)

    z <- tapply(newValues, oldValues, c, simplify=FALSE)

    rval <- lapply(rl$values, function(v) z[[v]])
    rval <- rlDecodeList(rval, rl$lengths)

    return(rval)
}