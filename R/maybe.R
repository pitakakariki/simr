maybeTest <- function(z=sample(1:4, 1)) {

  if(z == 1) return(1)

  if(z == 2) {

    warning("z is 2.")
    return(2)
  }

  if(z == 3) stop("z is 3!")

  if(z == 4) {

    warning("z is 4.")
    warning("No, really: z is 4.")
    return(4)
  }

  if(z > 4) {

    warning("I can't count that high.")
    stop("z is too big!")
  }

  stop("Impossible!!")

}

maybe <- function(f, returnName="value", warningName="warning", errorName="error")
  function(...) {

  returnValue <- NULL
  warningValue <- NULL
  errorValue <- NULL

  returnValue <- tryCatch(

    withCallingHandlers(eval.parent(f(...)),

    warning=function(w) {

      warningValue <<- append(warningValue, w$message)
      invokeRestart("muffleWarning")
    }),

    error=function(e) {

      errorValue <<- e$message
      return(NULL)
    }
  )

  rval <- list()
  class(rval) <- "Maybe"

  rval[returnName] <- list(returnValue)
  rval[warningName] <- list(warningValue)
  rval[errorName] <- list(errorValue)

  return(rval)
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

  z <- list()
  z[maybenot] <- llply(.data$errormessage[maybenot], function(e) maybe(stop(e))())
  z[!maybenot] <- llply(.data$value[!maybenot], maybe(.fun), ..., .progress=.progress)

  # $value
  rval <- list()
  rval $ value <- llply(z, `[[`, "value")

  # extract warnings and errors from $value?
  extractWarnings <- if(.extract) do.call(rbind, llply(rval$value, `[[`, "warnings")) else maybeFrame()
  extractErrors <- if(.extract) do.call(rbind, llply(rval$value, `[[`, "errors")) else maybeFrame()

  # $warnings
  warnings <- llply(z, `[[`, "warning")
  index <- rep(seq_along(warnings), laply(warnings, length))
  stage <- rep(.text, length(index))
  message <- unlist(warnings)

  rval $ warnings <- rbind(
    .data$warnings,
    extractWarnings,
    data.frame(stage, index, message, stringsAsFactors=FALSE)
  )

  # $errors
  errors <- llply(z, `[[`, "error")
  index <- which(!laply(errors, is.null))
  stage <- rep(.text, length(index))
  message <- unlist(errors)

  rval $ errors <- rbind(
    .data$errors,
    extractErrors,
    data.frame(stage, index, message, stringsAsFactors=FALSE)
  )

  class(rval) <- "maybeList"

  return(rval)
}

dim_ <- function(x) if(is.null(dim(x))) length(x) else dim(x)

ndim_ <- function(x) length(dim_(x))

drop_ <- function(x, n) {

    # check that n'th dimension has length one.
    if(length(dim_(x)[n]) != 1) stop("can't drop a dimension if length isn't one")

    dn <- dimnames(x)[-n]
    dim(x) <- dim(x)[-n]
    dimnames(x) <- dn

    return(x)
}

slice2 <- function(x, i) {

    drop_(x[i,,, drop=FALSE], 1)
}

slice2 <- function(x, i) {

    drop_(x[,i,, drop=FALSE], 2)
}

slice3 <- function(x, i) {

    drop_(x[,,i, drop=FALSE], 3)
}

list_to_atomic <- function(x) {

  # must be a list of length one things, with maybe some zeroes
  if(any(laply(x, length) > 1)) stop("vectors longer than one found")

  # they should probably be atomic too
  if(any(laply(x, is.recursive))) stop("recursive elements found")

  # nb NULL -> NA
  unlist(ifelse(laply(x, is.null), NA, x))
}

list_to_matrix <- function(x) {

# must be all the same length, with maybe some zeroes
    d <- unique(llply(x, dim_))
    if(0 %in% d) d <- d[-match(0, d)]
    if(length(d) != 1) stop("multiple dimensionalities found")
    d <- d[[1]]

    # nb NULL -> NA
    x <- ifelse(laply(x, is.null), list(array(NA, d)), x)

    # they should probably be atomic too
    if(any(laply(x, is.recursive))) stop("recursive elements found")

    simplify2array(x)
}

maybe_laply <- function(...) {

  # do maybe_llply stuff
  rval <- maybe_llply(...)

  # simplify and return
  rval $ value <- list_to_atomic(rval $ value)

  return(rval)
}

maybe_rlply <- function(.N, .thing, ...) {

  maybe_llply(seq_len(.N), eval.parent(substitute(function(.) .thing)), ...)
}

maybe_raply <- function(.N, .thing, ...) {

  maybe_laply(seq_len(.N), eval.parent(substitute(function(.) .thing)), ...)
}

### doesn't actually work???
#
# eg1 sometimes(confint, 0.5)(fm)
# eg2 sometimes(random(), 0.5)(fm) # uses a function factory
sometimes <- function(f, p=0.01) function(...) {

    r <- runif(1)
    if(r < p) stop(paste("x8x", r))
    #eval.parent(substitute(f(...)))
    f(...)
}
