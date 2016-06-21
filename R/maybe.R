tag <- function(thing, tag="") {

    tryCatch(

        withCallingHandlers(eval.parent(thing),

            warning=function(w) {

                w$tag <- tag
                warning(w)
                invokeRestart("muffleWarning")
            }),

        error=function(e) {

            e$tag <- tag
            stop(e)
        }
    )

    invisible(NULL)
}

maybe <- function(f) {

    function(...) {

        returnValue <- NULL
        warningValue <- NULL
        warningTag <- NULL
        errorValue <- NULL
        errorTag <- NULL

        returnValue <- tryCatch(

            withCallingHandlers(f(...),

                warning=function(w) {

                    warningValue <<- append(warningValue, w$message)
                    wtag <- if(is.null(w$tag)) "" else w$tag
                    warningTag <<- append(warningTag, wtag)
                    invokeRestart("muffleWarning")
                }),

            error=function(e) {

                errorValue <<- e$message
                errorTag <<- if(is.null(e$tag)) "" else e$tag
                return(NULL)
            }
        )

        rval <- list()
        class(rval) <- "Maybe"

        rval["value"] <- list(returnValue) # nb returnValue might be NULL
        rval["warning"] <- list(warningValue)
        rval["warningtag"] <- list(warningTag)
        rval["error"] <- list(errorValue)
        rval["errortag"] <- list(errorTag)

        return(rval)
    }
}

list2maybe <- function(x) {

    rval <- list()

    rval $ value <- as.list(x)

    rval $ warnings <- maybeFrame()
    rval $ errors <- maybeFrame()

    class(rval) <- "maybeList"

    return(rval)
}

maybeFrame <- function() {

    data.frame(stage=character(), index=integer(), message=character(), stringsAsFactors=FALSE)
}

maybe_llply <- function(.data, .fun, .text="", ..., .progress=progress_simr(.text), .extract=FALSE) {

    if(!is(.data, "maybeList")) {

        .data <- list2maybe(.data)
    }

    maybenot <- seq_along(.data$value) %in% .data$errors$index

    .parallel = getSimrOption("parallel")
    .paropts = getSimrOption("paropts")

    if(.parallel) {
        # plyr doesn't support progress bars with parallel execution
        if(getSimrOption("progress")){
            message("Progress disabled when using parallel computation")
        }
        .progress <- "none"
    }

    z <- list()
    # not sure if parallel provides a substantial boost here
    z[maybenot] <- llply(.data$errormessage[maybenot], function(e) maybe(stop(e))(),.parallel=.parallel,.paropts=.paropts)
    # this is the big expensive function, so use parallel if enabled
    z[!maybenot] <- llply(.data$value[!maybenot], maybe(.fun), ..., .progress=.progress, .parallel=.parallel,.paropts=.paropts)

    # $value
    rval <- list()
    rval $ value <- llply(z, `[[`, "value")

    # extract warnings and errors from $value?
    # don't use parallel computation for these simple operations because the overhead is greater than the gain
    extractWarnings <- if(.extract) do.call(rbind, llply(rval$value, `[[`, "warnings")) else maybeFrame()
    extractErrors <- if(.extract) do.call(rbind, llply(rval$value, `[[`, "errors")) else maybeFrame()

    # $warnings
    # don't use parallel computation for these simple operations because the overhead is greater than the gain
    warnings <- llply(z, `[[`, "warning")
    wtags <- llply(z, `[[`, "warningtag")
    index <- rep(seq_along(warnings), laply(warnings, length))
    #stage <- rep(.text, length(index))
    message <- unlist(warnings)
    stage <- unlist(wtags)

    rval $ warnings <- rbind(
        .data$warnings,
        extractWarnings,
        data.frame(stage, index, message, stringsAsFactors=FALSE)
    )

    # $errors
    # don't use parallel computation for these simple operations because the overhead is greater than the gain
    errors <- llply(z, `[[`, "error")
    etags <- llply(z, `[[`, "errortag")
    index <- which(!laply(errors, is.null))
    #stage <- rep(.text, length(index))
    message <- unlist(errors)
    stage <- unlist(etags)

    rval $ errors <- rbind(
        .data$errors,
        extractErrors,
        data.frame(stage, index, message, stringsAsFactors=FALSE)
    )

    class(rval) <- "maybeList"

    return(rval)
}


list_to_atomic <- function(x) {

    # must be a list of length one things, with maybe some zeroes
    if(any(laply(x, length) > 1)) stop("vectors longer than one found")

    # they should probably be atomic too
    if(any(laply(x, is.recursive))) stop("recursive elements found")

    # nb NULL -> NA
    unlist(ifelse(laply(x, is.null), NA, x))
}

maybe_laply <- function(...) {

    # do maybe_llply stuff
    rval <- maybe_llply(...)

    # simplify and return
    rval $ value <- list_to_atomic(rval $ value)

    return(rval)
}

maybe_raply <- function(.N, .thing, ...) {

    maybe_laply(seq_len(.N), eval.parent(substitute(function(.) .thing)), ...)
}

maybe_rlply <- function(.N, .thing, ...) {

    maybe_llply(seq_len(.N), eval.parent(substitute(function(.) .thing)), ...)
}

sometimes <- function(x, p=0.01, emsg="x8x", pw=NA, wmsg="boo!", lambda=NA) {

    if(!is.na(pw)) {

        if(runif(1) < pw) {

            nmsg <- if(is.na(lambda)) 1 else rpois(1, lambda)

            for(i in seq_len(nmsg)) {

                warning(sample(wmsg, 1))
            }
        }
    }

    if(runif(1) < p) test_error(emsg)

    x
}

test_error <- function(e) stop(e)