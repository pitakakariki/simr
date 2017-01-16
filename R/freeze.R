





frozen <- function(object) {

    if(is.data.frame(object)) attr(object, "simrFrozen") else frozen(getData(object))
}


`frozen<-` <- function(object, value) {

    if(is.list(value)) {

        if(!all(sort(names(value)==c("i", "y")))) stop("Frozen value should be a list with entries 'i' and 'y'.")

    } else {

        X <- getData(object)
        response <- eval(formula(object)[[2]], X, environment(formula(object)))

        if(!all(value %in% seq_len(nrow(X)))) stop("Supplied values are outside range.")

        value <- list(i=value, y=response[value])
    }

    if(is.data.frame(object)) {

        attr(object, "simrFrozen") <- value

    } else {

        frozen(getData(object)) <- value
    }

    return(object)
}

freeze <- function(y, object) {

    fr <- frozen(object)

    if(!is.null(fr)) {

        if(is.matrix(y)) {

            y[fr$i,] <- fr$y

        } else {

            y[fr$i] <- fr$y
        }
    }

    return(y)
}




