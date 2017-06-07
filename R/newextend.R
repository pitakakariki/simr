
newextend_ <- function(object, along, n, addn, values, addvalues, where) {

    if(missing(along)) stop("You need to specity a variable with 'along' or 'within'")

    eval(bquote(newextend(object, .(along), n, addn, values, addvalues, where)))
}

#### Get rid of NSE, maybe think about adding it later?

newextend <- function(object, along, n, addn, values, addvalues, where) {

    if(missing(along)) stop("You need to specity a variable with 'along' or 'within'")
    along <- nse(along)

    if(missing(where)) where <- rep(TRUE, nrow(object)) else {

        e <- substitute(where)
        where <- eval(e, object, parent.frame())
    }

    if(is.logical(where)) {

        if(length(where) != nrow(object)) stop("Length of 'where' does not match number of rows.")

    } else {

        where <- seq_len(nrow(object)) %in% where
    }

    # if(!nrow(object)) stop...

    x <- object[[along]]

    factorCheck <- if(is.factor(x)) {

        x <- as.character(x)
        TRUE

    } else FALSE

    # work out the new values

    valueMap <- makeValueMap(x, n, addn, values, addvalues, where)

    # work out which rows to base them on

    rle <- rlEncode(x, where)
    mapping <- split(valueMap$newValues, valueMap$oldValues)
    #newRle <- lapply(oldRle$values, function(val) mapping[[val]])
    rle$values <- lapply(rle$values, getElement, object=mapping)
    newVariable <- rlDecodeList(rle)

    # nb: newVariable $ values:  the values for the new variable
    #                 $ indices: the rows to copy from the old frame

    # build a new data frame

    newData <- object[newVariable$indices, , drop=FALSE]
    newData[[along]] <- newVariable$values

    # cleanup?

    if(factorCheck) newData[[along]] <- as.factor(newData[[along]])

    return(newData)
}

makeValueMap <- function(x, n, addn, values, addvalues, where) {

    m <- missing(n) + missing(addn) + missing(values) + missing(addvalues)

    if(m==4) stop("Extended values not specified.")
    if(m!=3) stop("Only one argument from [n|addn|values|addvalues] can be specified.")

    startValues <- unique(x)
    nStart <- length(startValues)
    lastValue <- tail(sort(unique(x[where])), 1)

    # n
    if(!missing(n)) {

        if(n > nStart) addn <- n - length(startValues) else newValues <- startValues[seq_len(n)]
    }

    # addn
    if(!missing(addn)) {

        newValues <- c(startValues, makeNewValues(startValues, addn))
    }

    # values
    if(!missing(values)) {

        newValues <- values
    }

    # addvalues
    if(!missing(addvalues)) {

        newValues <- c(startValues, addvalues)
        #oldValues <- c(startValues, rep(lastValue, length(addvalues)))
    }

    # now oldValues

    # using == "last"

    s <- seq_len(min(length(startValues), length(newValues)))

    oldValues <- newValues
    oldValues[] <- lastValue
    oldValues[s] <- startValues[s]


    list(oldValues=oldValues, newValues=newValues)
}

makeNewValues <- function(x, n) {

    if(is.numeric(x)) {

        first <- x[1]
        last <- tail(x, 1)

        step <- (last - first) / (length(x) - 1)

        return(last + step * seq_len(n))

    } else {

        if(length(x) > 1) {

            # Try to detect a pattern

            f <- function(i) length(unique(substr(x, 1, i)))
            test <- sapply(seq_len(1+min(nchar(x)))-1, f)
            i <- max(which(test == 1))-1

            pattern <- substr(x[1], 1, i)
            sequence <- substr(x, i+1, max(nchar(x)))

            # is the sequence numbers?

            suppressWarnings(numSeq <- as.numeric(sequence))
            if(!any(is.na(numSeq)) & length(unique(diff(numSeq)))==1) {

                newSeq <- tail(numSeq, 1) + diff(numSeq[1:2]) * seq_len(n)
                return(paste0(pattern, newSeq))
            }

            # is the sequence letters?
            if(all(nchar(sequence)==1)) {

                for(charset in list(letters, LETTERS)) {

                    if(all(sequence %in% charset)) {

                        numSeq <- match(sequence, charset)
                        if(length(unique(diff(numSeq)))==1) {

                            newSeq <- tail(numSeq, 1) + diff(numSeq[1:2]) * seq_len(n)
                            if(all(newSeq <= 26)) {

                                return(paste0(pattern, charset[newSeq]))
                            }
                        }
                    }
                }
            }
        }

        if(mean(nchar(x)) <= 3) {

            # short: a, b, c

            rval <- character(n)
            suppressWarnings(rval[] <- letters)

            return(make.unique(rval))

        } else {

            # long: New Level 1, New Level 2, ...

            return(paste("New Level", seq_len(n)))
        }
    }
}

seqTest <- function(x, n) c(x, makeNewValues(x, n))

# use this instead of deparse(substitute(x))
# backwards compatability, both of these should work:
#     along="x"
#     along=x
nse <- function(x) {

    #x <- deparse(substitute(x)), but one level up
    x <- substitute(x) # e.g. along
    x <- bquote(substitute(.(x))) # e.g. substitute(along)
    x <- eval.parent(x) # e.g varname
    x <- deparse(x) # e.g. "varname"

    # get rid of ""

    n <- nchar(x)

    first <- substr(x, 1, 1)
    last <- substr(x, n, n)

    if(first=="\"" && last=="\"") substr(x, 2, n-1) else x
}

rlEncode <- function (x, s=rep(TRUE, length(x))) {

    n <- length(x)

    y <- tail(x, -1) != head(x, -1)
    i <- c(which(y | is.na(y)), n)

    values <- x[i]
    lengths <- diff(c(0, i))
    sList <- relist(s, lapply(lengths, rep, x=0))

    rval <- list(

        values = values,
        lengths = lengths,
        sList = sList
    )

    structure(rval, class="rlEncode")
}

print.rlEncode <- function(x) {

    xprint <- data.frame(values=x$values, lengths=x$lengths)
    xprint$sList <- lapply(x$sList, as.numeric)

    print(xprint)
}

rlDecodeList <- function(rle) {

    nT <- sapply(rle$sList, sum)
    nF <- rle$lengths - nT
    n <- sum(lengths(rle$values) * nT + nF)

    values <- logical(n)
    indices <- integer(n)

    ix_new <- 0
    ix_old <- 0

    for(i in seq_along(rle$values)) {

        for(j in seq_along(rle$values[[i]])) {

            for(k in seq_len(rle$lengths[i])) {

                if(j > 1 && !rle$sList[[i]][k]) next

                ix_new <- ix_new + 1

                values[ix_new] <- rle$values[[i]][[j]]
                indices[ix_new] <- ix_old + k
            }
        }

        ix_old <- ix_old + rle$lengths[i]
    }

    list(values=values, indices=indices)
}


