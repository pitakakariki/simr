rlEncode <- function (x) {

    n <- length(x)

    y <- tail(x, -1) != head(x, -1)
    i <- c(which(y | is.na(y)), n)

    list(

        values = x[i],
        lengths = diff(c(0, i))
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

newextend <- function(object, along, n, addn, values, addvalues, where) {

    # if(!nrow(object)) stop...

    x <- object[[along]]

    factorCheck <- if(is.factor(x)) {

        x <- as.character(x)
        TRUE

    } else FALSE

    # work out the new values

    valueMap <- makeValueMap(x, n, addn, values, addvalues)

    oldValues <- valueMap$oldValues
    newValues <- valueMap$newValues

    # work out which rows to base them on

    oldRle <- rlEncode(x)
    mapping <- split(newValues, oldValues)
    #newRle <- lapply(oldRle$values, function(val) mapping[[val]])
    newRle <- lapply(oldRle$values, getElement, object=mapping)
    newVariable <- rlDecodeList(newRle, oldRle$lengths)

    # nb: newVariable $ values:  the values for the new variable
    #                 $ indices: the rows to copy from the old frame

    # build a new data frame

    newData <- object[newVariable$indices, ]
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
    lastValue <- sort(startValues)[nStart]

    # n
    if(!missing(n)) {

        if(n > nStart) addn <- n - length(startValues) else newValues <- startValues[seq_len(n)]
    }

    # addn
    if(!missing(addn)) {

        newValues <- c(startValues, paste0("x", seq_len(addn)))
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

    newValues <- make.unique(newValues)

    # now oldValues

    # where == "last"

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

        # Try to detect a pattern

        f <- function(i) length(unique(substr(x, 1, i)))
        test <- sapply(seq_len(1+min(nchar(x)))-1, f)
        i <- max(which(test == 1))

        pattern <- substr(x[1], 1, i)
        sequence <- substr(x, i+1, max(nchar(x)))

        if(pattern != "") {



        }

        if(mean(nchar(x)) <= 3) {

            # short: a, b, c

            rval <- character(n)
            suppressWarnings(rval[] <- letters)

            return(rval)

        } else {

            # long: New Level 1, New Level 2, ...

            return(paste("New Level", seq_len(n)))
        }
    }
}

findPattern <- function(x, n) {

    f <- function(i) length(unique(substr(x, 1, i)))

    test <- sapply(seq_len(1+min(nchar(x)))-1, f)

    i <- max(which(test == 1))-1

    substr(x[1], 1, i)
}






