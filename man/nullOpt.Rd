\name{nullOpt}
\alias{nullOpt}
\title{'Fit' a mixed model by returning previously calculated parameters.}
\usage{
nullOpt(fn, par, lower, upper, control)
}
\description{
Dummy 'optimizer' that allows `lmer` to be called without
re-estimating parameters.
}

