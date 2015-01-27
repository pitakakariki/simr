data(frogs)
pferUMF <- unmarkedFrameOccu(pfer.bin)
plot(pferUMF, panels=4)
# add some fake covariates for illustration
siteCovs(pferUMF) <- data.frame(sitevar1 = rnorm(numSites(pferUMF)))
# observation covariates are in site-major, observation-minor order
obsCovs(pferUMF) <- data.frame(obsvar1 = rnorm(numSites(pferUMF) * obsNum(pferUMF)))
fm <- occu(~ obsvar1 ~ 1, pferUMF)
confint(fm, type= 'det' , method = 'normal' )
confint(fm, type= 'det' , method = 'profile' )
# estimate detection effect at obsvars=0.5
lc <- linearComb(fm[ 'det' ],c(1,0.5))
# transform this to probability (0 to 1) scale and get confidence limits
btlc <- backTransform(lc)
confint(btlc, level = 0.9)
# Empirical Bayes estimates of proportion of sites occupied
re <- ranef(fm)
sum(bup(re, stat="mode"))

unmarked_occupancy_test <- function(fm, ...) {

    capture.output(

        rval <- show(fm@estimates@estimates$state)[["P(>|z|)"]]
    )

    return(rval)
}

unmarked_detection_test <- function(fm, xname) {

    capture.output(

        rval <- show(fm@estimates@estimates$det)[xname, "P(>|z|)"]
    )

    return(rval)
}

doFit.unmarkedFitOccu <- function(y, fm, ...) {

    call <- fm@call

    newData <- fm@data
    newData@y <- y
    call$data <- newData

    eval(call)
}

fixef.unmarkedFitOccu <- function(fm) coef(fm, "det", FALSE)
