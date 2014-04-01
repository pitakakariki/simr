\name{fixef<-}
\alias{fixef<-}
\title{Modify fixed effect coefs.}
\usage{
fixef(m)[index] <- value
}
\arguments{
  \item{object}{a mixed-effects model (merMod) object.}

  \item{value}{a new vector of fixed effect coefficients.}
}
\description{
This function replaces the fixed effect coefficients in a
fitted model.
}
\details{
This function would normally be used to change the value of
individual fixed effect coefficients.
}
\examples{
m <- lmer(Carbon ~ Year + (Year | Cluster), kiwifruit)
fixef(m)
fixef(m)['Year'] <- -0.13
fixef(m)
}

