#
# Default settings.
#

.simrOptions <- new.env(parent=emptyenv())

.simrOptions $ nSim <- 1000
.simrOptions $ light <- 150
.simrOptions $ progress <- TRUE
.simrOptions $ binom <- "exact"

#' Options Settings for \code{simr}
#'
#' Control the default behaviour of \code{simr} analyses.
#' 
#' @param ...
#' @param x   option name (character string)
#' 
#' @return
#' 
#' \code{getSimrOption} returns the current value for the option \code{x}.
#' 
#' \code{simrOptions} returns
#' 
#' \enumerate{
#' \item a named list of all options, if no arguments are given.
#' \item a named list of specified options, if a list of option names is given.
#' \item (invisibly) a named list of changed options with their previous values, if options are set.
#' }
#' 
#' @section Options in \code{simr}:
#' 
#' There are options
#' 
#' @examples
#' 
#' getSimrOption("nSim")
#' oldopts <- simrOptions(nSim=5)
#' getSimrOption("nSim")
#' simrOptions(oldopts)
#' getSimrOption("nSim")
#' 
#' @export
simrOptions <- function(...) {
  
  args <- list(...)
  
  # Case 1: empty list; return all options.
  if(length(args) == 0) return(as.list(.simrOptions))
  
  # Case 2: unnamed list ...
  if(is.null(names(args))) {

    # Case 2a: single argument which is a list; use do.call.
    if(length(args) == 1 && is.list(args[[1]])) return(do.call(simrOptions, args[[1]]))
    
    # Case 2b: must be a list of characters; return specified options.
    optNames <- unlist(args)
    if(!is.character(optNames)) stop("not a list of option names")
    for(n in optNames) if(!(n %in% ls(.simrOptions))) stop(str_c("no option to get named ", n))
    return(mget(optNames, envir=.simrOptions))
  }

  # must be either unnamed or fully named
  if(any(nchar(names(args))==0)) stop("cannot get and set options at the same time")

  # Case 3: named list; set options.
  for(n in names(args)) if(!n %in% ls(.simrOptions)) stop(str_c("no option to set named ", n))
    
  oldOptions <- mget(names(args), envir=.simrOptions)
  mapply(assign, names(args), args, MoreArgs=list(envir=.simrOptions))
  invisible(oldOptions)
}

#' @export
#' @rdname simrOptions
getSimrOption <- function(x) simrOptions(x)[[1]]

