## --> ../man/profile-methods.Rd

##' @importFrom splines backSpline interpSpline periodicSpline
##' @importFrom stats profile
##' @method profile merMod
##' @export
profile.workaround <- function(fitted, which=1:nptot, alphamax = 0.01,
    		   maxpts = 100, delta = cutoff/8,
                           verbose=0, devtol=1e-9,
                           maxmult = 10,
                           startmethod = "prev",
                           optimizer = "bobyqa",
                           signames = TRUE, ...)
{
  ## FIXME: allow choice of nextstep/nextstart algorithm?
  ## FIXME: by default, get optimizer from within fitted object
  ## FIXME: allow selection of individual variables to profile by name?
  ## FIXME: allow for failure of bounds (non-pos-definite correlation matrices) when >1 cor parameter
  ## FIXME: generalize to GLMMs
  ## (use different devfun;
  ##  be careful with scale parameter;
  ##  profile all parameters at once rather than RE first and then fixed)

    useSc <- isLMM(fitted) || isNLMM(fitted)
    dd <- lme4:::devfun2(fitted,useSc,signames)
    ## FIXME: figure out to what do here ...
    if (isGLMM(fitted) && fitted@devcomp$dims["useSc"])
        stop("can't (yet) profile GLMMs with non-fixed scale parameters")
    stopifnot(devtol >= 0)
    base <- attr(dd, "basedev")
    thopt <- attr(dd, "thopt")
    stderr <- attr(dd, "stderr")
    pp <- environment(dd)$pp
    X.orig <- pp$X
    n <- environment(dd)$n
    p <- length(pp$beta0)

    opt <- attr(dd, "optimum")
    nptot <- length(opt)
    nvp <- nptot - p    # number of variance-covariance pars
    wi.vp <- seq_len(nvp)
    if(nvp > 0) fe.orig <- opt[- wi.vp]

    which <- lme4:::get.which(which,nvp,nptot,names(opt),verbose)

    ans <- lapply(opt[which], function(el) NULL)
    bakspl <- forspl <- ans

    res <- c(.zeta = 0, opt)
    res <- matrix(res, nrow = maxpts, ncol = length(res),
                  dimnames = list(NULL, names(res)), byrow = TRUE)
    ## FIXME: why is cutoff based on nptot (i.e. boundary of simultaneous LRT conf region for nptot values)
    ##  when we are computing (at most) 2-parameter profiles here?

    cutoff <- sqrt(qchisq(1 - alphamax, nptot))

    ## helper functions

    ## nextpar calculates the next value of the parameter being
    ## profiled based on the desired step in the profile zeta
    ## (absstep) and the values of zeta and column cc for rows
    ## r-1 and r.  The parameter may not be below lower (or above upper)
    nextpar <- function(mat, cc, r, absstep,
                        lower = -Inf, upper = Inf, minstep=1e-6) {
        rows <- r - (1:0)         # previous two row numbers
        pvals <- mat[rows, cc]
        zeta <- mat[rows, ".zeta"]
        num <- diff(pvals)
        if (is.na(denom <- diff(zeta)) || denom==0) {
            warning("Last two rows have identical or NA .zeta values: using minstep")
            step <- minstep
        } else {
            step <- absstep*num/denom
            if (r>1) {
                if (abs(step) > (maxstep <- abs(maxmult*num))) {
                    maxstep <- sign(step)*maxstep
                    if (verbose) cat(sprintf("capped step at %1.2f (multiplier=%1.2f > %1.2f)\n",
                                             maxstep,abs(step/num),maxmult))
                    step <- maxstep
                }
            }
        }
        min(upper, max(lower, pvals[2] + sign(num) * step))
      }

    nextstart <- function(mat, pind, r, method="global") {
      ## FIXME: indexing may need to be checked (e.g. for fixed-effect parameters)
      switch(method,
             global=opt[seqpar1][-pind],  ## address opt, no zeta column
             prev=mat[r,1+seqpar1][-pind],
             extrap=stop("stub")) ## do something with mat[r-(1:0),1+seqnvp])[-pind] ...
    }

    ## mkpar generates the parameter vector of theta and
    ## sigma from the values being profiled in position w
    mkpar <- function(np, w, pw, pmw) {
        par <- numeric(np)
        par[w] <- pw
        par[-w] <- pmw
        par
    }

    ## fillmat fills the third and subsequent rows of the matrix
    ## using nextpar and zeta
### FIXME:  add code to evaluate more rows near the minimum if that
###        constraint was active.
    fillmat <- function(mat, lowcut, upcut, zetafun, cc) {
        nr <- nrow(mat)
        i <- 2L
        while (i < nr && mat[i, cc] > lowcut && mat[i,cc] < upcut &&
               (is.na(curzeta <- abs(mat[i, ".zeta"])) || curzeta <= cutoff)) {
            np <- nextpar(mat, cc, i, delta, lowcut, upcut)
            ns <- nextstart(mat, cc-1, i, startmethod)
            mat[i + 1L, ] <- zetafun(np,ns)
            if (verbose>0) {
                cat(i,cc,mat[i+1L,],"\n")
            }
            i <- i + 1L
        }
        if (mat[i-1,cc]==lowcut) {
            ## fill in more values near the minimum
        }
        if (mat[i-1,cc]==upcut) {
            ## fill in more values near the maximum
        }

        mat
    }

    ## bounds on Cholesky: [0,Inf) for diag, (-Inf,Inf) for off-diag
    ## bounds on sd-corr:  [0,Inf) for diag, (-1.0,1.0) for off-diag
    lower <- pmax(fitted@lower,-1.0)
    upper <- 1/(fitted@lower != 0)## = ifelse(fitted@lower==0, Inf, 1.0)
    if (useSc) { # bounds for sigma
        lower <- c(lower,0)
        upper <- c(upper,Inf)
    }
    ## bounds on fixed parameters (TODO: allow user-specified bounds, e.g. for NLMMs)
    lower <- c(lower,rep.int(-Inf, p))
    upper <- c(upper, rep.int(Inf, p))
    npar1 <- if (isLMM(fitted)) nvp else nptot
    ## check that devfun2() computation for the base parameters is (approx.) the
    ##  same as the original devfun() computation
    if(!isTRUE(all.equal(unname(dd(opt[seq(npar1)])), base, tolerance=1e-5))){
        stop("Profiling over both the residual variance and\n",
             "fixed effects is not numerically consistent with\n",
             "profiling over the fixed effects only")}

    ## sequence of variance parameters to profile
    seqnvp <- intersect(seq_len(npar1),which)
    ## sequence of 'all' parameters
    seqpar1 <- seq_len(npar1)
    lowvp <- lower[seqpar1]
    upvp <- upper[seqpar1]
    form <- .zeta ~ foo           # pattern for interpSpline formula

    for (w in seqnvp) {
       if (verbose) cat(if(isLMM(fitted)) "var-cov " else "", "parameter ",w,":\n",sep="")
       wp1 <- w + 1L
       start <- opt[seqpar1][-w]
       pw <- opt[w]
       lowcut <- lower[w]
       upcut <- upper[w]
       zeta <- function(xx,start) {
	   ores <- tryCatch(lme4:::optwrap(optimizer, par=start,
				    fn=function(x) dd(mkpar(npar1, w, xx, x)),
				    lower = lowvp[-w],
				    upper = upvp [-w]), error=function(e)NULL)
	   if (is.null(ores)) {
	       devdiff <- NA
	       pars <- NA
	   } else {
	       devdiff <- ores$fval - base
	       pars <- ores$par
	   }
           if (is.na(devdiff)) {
               warning("NAs detected in profiling")
           } else {
               if(verbose && devdiff < 0)
                   cat("old deviance ",base,",\n",
                       "new deviance ",ores$fval,",\n",
                       "new params ",
                       paste(mkpar(npar1,w,xx,ores$par),
                             collapse=","),"\n")
               if (devdiff < (-devtol))
                   stop("profiling detected new, lower deviance")
               if(devdiff < 0)
                   warning(gettextf("slightly lower deviances (diff=%g) detected",
                                    devdiff), domain=NA)
           }
           devdiff <- max(0,devdiff)
           zz <- sign(xx - pw) * sqrt(devdiff)
           r <- c(zz, mkpar(npar1, w, xx, pars))
           if (isLMM(fitted)) c(r, pp$beta(1)) else r
       }## {zeta}

### FIXME: The starting values for the conditional optimization should
### be determined from recent starting values, not always the global
### optimum values.

### Can do this most easily by taking the change in the other parameter values at
### the two last points and extrapolating.


       ## intermediate storage for pos. and neg. increments
       pres <- nres <- res
       ## assign one row, determined by inc. sign, from a small shift
       ## FIXME:: do something if pw==0 ???
       shiftpar <- if (pw==0) 1e-3 else pw*1.01
       ## Since both the pos- and neg-increment matrices are already
       ## filled with the opt. par. results, this sets the first
       ## two rows of the positive-increment matrix
       ## to (opt. par, shiftpar) and the first two rows of
       ## the negative-increment matrix to (shiftpar, opt. par),
       ## which sets up two points going in the right direction
       ## for each matrix (since the profiling algorithm uses increments
       ## between rows to choose the next parameter increment)
       nres[1, ] <- pres[2, ] <- zeta(shiftpar, start=opt[seqpar1][-w])
       ## fill in the rest of the arrays and collapse them
       upperf <- fillmat(pres, lowcut, upcut, zeta, wp1)
       if (pw>lowcut) {
           lowerf <- fillmat(nres, lowcut, upcut, zeta, wp1)
       } else {
           ## don't bother to fill in 'nres' matrix
           lowerf <- nres
       }
       ## this will throw away the extra 'opt. par' and 'shiftpar'
       ## rows introduced above:
       bres <- as.data.frame(unique(rbind2(upperf,lowerf)))
       pname <- names(opt)[w]
       bres$.par <- pname
       ans[[pname]] <- bres[order(bres[, wp1]), ]
       form[[3]] <- as.name(pname)

       ## FIXME: test for bad things here??
       bakspl[[pname]] <- tryCatch(splines:::backSpline(forspl[[pname]] <-
                                              splines:::interpSpline(form, bres,na.action=na.omit)),
                                   error=function(e)e)
       if (inherits(bakspl[[pname]],"error")) {
           warning("non-monotonic profile")
     }

    } ## for(w in ..)

    ## profile fixed effects separately (for LMMs)
    if (isLMM(fitted)) {
        offset.orig <- fitted@resp$offset
        fp <- seq_len(p)
        fp <- fp[(fp+nvp) %in% which]
        for (j in fp) {
            if (verbose) cat("fixed-effect parameter ",j,":\n",sep="")
            pres <-            # intermediate results for pos. incr.
                nres <- res    # and negative increments
            est <- opt[nvp + j]
            std <- stderr[j]
            Xw <-X.orig[, j, drop=TRUE]
            Xdrop <- lme4:::.modelMatrixDrop(X.orig, j)
            pp1 <- do.call("new", list(Class = class(pp),
                                     X = Xdrop,
                                     Zt = pp$Zt,
                                     Lambdat = pp$Lambdat,
                                     Lind = pp$Lind,
                                     theta = pp$theta,
                                     n = nrow(Xdrop))
                           )
### FIXME Change this to use the deep copy and setWeights, setOffset, etc.
            rr <- new(Class=class(fitted@resp), y=fitted@resp$y)
            rr$setWeights(fitted@resp$weights)
            fe.zeta <- function(fw, start) {
                ## (start parameter ignored)
                rr$setOffset(Xw * fw + offset.orig)
                rho <- as.environment(list(pp=pp1, resp=rr))
                parent.env(rho) <- parent.frame()
                ores <- lme4:::optwrap(optimizer,
                                par=thopt, fn=lme4:::mkdevfun(rho, 0L),
                                lower = fitted@lower)
                ## ?? this optimization is done on the ORIGINAL
                ## theta scale (i.e. not the sigma/corr scale ??
                ## upper=Inf for all cases
                ## lower = pmax(fitted@lower, -1.0),
                ## upper = 1/(fitted@lower != 0))## = ifelse(fitted@lower==0, Inf, 1.0)
                fv <- ores$fval
                sig <- sqrt((rr$wrss() + pp1$sqrL(1))/n)
                c(sign(fw - est) * sqrt(fv - base),
                  lme4:::Cv_to_Sv(ores$par, vapply(fitted@cnms,length, 1), s=sig),
                  ## ores$par * sig, sig,
                  mkpar(p, j, fw, pp1$beta(1)))
            }
            nres[1, ] <- pres[2, ] <- fe.zeta(est + delta * std)
            poff <- nvp + 1L + j
            arg1 <- fillmat(pres,-Inf, Inf, fe.zeta, poff)
            arg2 <- fillmat(nres,-Inf, Inf, fe.zeta, poff)
            bres <- as.data.frame(unique(rbind2(arg1, arg2)))
            thisnm <- names(fe.orig)[j]
            bres$.par <- thisnm
            ans[[thisnm]] <- bres[order(bres[, poff]), ]
            form[[3]] <- as.name(thisnm)
            bakspl[[thisnm]] <-
                tryCatch(splines:::backSpline(forspl[[thisnm]] <- splines:::interpSpline(form, bres)),
                         error=function(e)e)
            if (inherits(bakspl[[thisnm]],"error")) warning("non-monotonic profile")
        } ## for(j in 1..p)
    } ## if isLMM

    ans <- do.call(rbind, ans)
    row.names(ans) <- NULL
    ans$.par <- factor(ans$.par)
    attr(ans, "forward") <- forspl
    attr(ans, "backward") <- bakspl
    class(ans) <- c("thpr", "data.frame")
    attr(ans, "lower") <- lower[seqnvp]
    attr(ans, "upper") <- upper[seqnvp]
    ans
} ## profile.merMod

