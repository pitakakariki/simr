#
# Default settings.
#

.simrOptions <- new.env(parent=emptyenv())

.simrOptions $ nsim <- 1000
.simrOptions $ alpha <- 0.05
.simrOptions $ progress <- TRUE
.simrOptions $ parallel <- FALSE
.simrOptions $ paropts <- NULL
.simrOptions $ binom <- "exact"
.simrOptions $ pbnsim <- 100
.simrOptions $ pcmin <- 3
.simrOptions $ pcmax <- 10
.simrOptions $ observedPowerWarning <- TRUE
.simrOptions $ carTestType <- "II"
.simrOptions $ lmerTestDdf <- "Satterthwaite"
.simrOptions $ lmerTestType <- 2

#' Options Settings for \code{simr}
#'
#' Control the default behaviour of \code{simr} analyses.
#'
#' @param ... a list of names to get options, or a named list of new values to set options.
#' @param opt option name (character string).
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
#' Options that can be set with this method (and their default values).
#'
#' \describe{
#'   \item{\code{nsim}}{default number of simulations (\code{1000}).}
#'   \item{\code{alpha}}{default confidence level (\code{0.05}).}
#'   \item{\code{progress}}{use progress bars during calculations (\code{TRUE}). Note that no progress bars are available if \code{parallel = TRUE}}
#'   \item{\code{parallel}}{apply function in parallel, using parallel backend provided by
#'                          \code{\link[foreach:foreach]{foreach}} (\code{FALSE}).
#'                          See \code{.parallel} in the \code{\link[plyr:llply]{llply}} documentation.}
#'   \item{\code{paropts}}{a list of additional options passed into the \code{\link[foreach:foreach]{foreach}}
#'                          function when parallel computation is enabled (\code{NULL}).
#'                          See \code{.paropts} in the \code{\link[plyr:llply]{plyr::llply}} documentation.}
#'   \item{\code{binom}}{method for calculating confidence intervals (\code{"exact"}).}
#'   \item{\code{pbnsim}}{number of simulations for parametric bootstrap tests using \code{pbkrtest} (\code{100}).}
#'   \item{\code{pcmin}}{minimum number of levels for the smallest point on a \code{\link{powerCurve}} (3).}
#'   \item{\code{pcmax}}{maximum number of points on the default \code{\link{powerCurve}} (10).}
#'   \item{\code{observedPowerWarning}}{warn if an unmodified fitted model is used (TRUE).}
#'   \item{\code{carTestType}}{ type of test, i.e. type of sum of squares, for tests performed with \code{\link[=Anova]{car::Anova}} (\code{"II"}).}
#'   \item{\code{lmerTestDdf}}{ approximation to use for denominator degrees of
#'                             freedom for tests performed with
#'                             \code{\link[lmerTest:lmer]{lmerTest}}
#'                             (\code{"Satterthwaite"}). Note that setting this
#'                             option to \code{"lme4"} will reduce the
#'                             \code{lmerTest} model to an \code{lme4} model and
#'                             break functionality based on \code{lmerTest}.}
#'  \item{\code{lmerTestType}}{ type of test, i.e. type of sum of squares, for
#'                              F-tests performed with
#'                              \code{\link[lmerTest:anova.merModLmerTest]{lmerTest::anova}}
#'                              (\code{2}). Note that unlike the tests performed
#'                              with \code{car::Anova}, the test type must be
#'                              given as a number and not a character.}
#'
#' }
#'
#' @examples
#'
#' getSimrOption("nsim")
#' oldopts <- simrOptions(nsim=5)
#' getSimrOption("nsim")
#' simrOptions(oldopts)
#' getSimrOption("nsim")
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
getSimrOption <- function(opt) simrOptions(opt)[[1]]

