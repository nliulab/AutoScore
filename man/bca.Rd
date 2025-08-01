% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bca.R
\name{bca}
\alias{bca}
\title{Bias-corrected and accelerated confidence intervals}
\usage{
bca(theta, conf.level = 0.95)
}
\arguments{
\item{theta}{a vector that contains draws of a quantity of interest using bootstrap samples.
The length of \code{theta} is equal to the number of iterations in the previously-run
bootstrap simulation.}

\item{conf.level}{the level of the desired confidence interval, as a proportion. Defaults to
.95 which returns the 95 percent confidence interval.}
}
\value{
returns a vector of length 2 in which the first element is the lower bound and the
second element is the upper bound
}
\description{
This function is taken from the 'coxed' package version 0.3.3 (archived on
CRAN). It is included here without modification solely because the package
has been removed from CRAN. Original authorship and credit belong to the
developers of the 'coxed' package. Source:
https://cran.r-project.org/package=coxed (archived)
}
\details{
This function uses the method proposed by DiCiccio and Efron (1996)
to generate confidence intervals that produce more accurate coverage
rates when the distribution of bootstrap draws is non-normal.
This code is adapted from the \code{BC.CI()} function within the
\code{\link[mediation]{mediate}} function in the \code{mediation} package.

\eqn{BC_a} confidence intervals are typically calculated using influence statistics
from jackknife simulations. For our purposes, however, running jackknife simulation in addition
to ordinary bootstrapping is too computationally expensive. This function follows the procedure
outlined by DiCiccio and Efron (1996, p. 201) to calculate the bias-correction and acceleration
parameters using only the draws from ordinary bootstrapping.
}
\references{
DiCiccio, T. J. and B. Efron. (1996). Bootstrap Confidence Intervals. \emph{Statistical Science}.
11(3): 189–212. \url{https://doi.org/10.1214/ss/1032280214}
}
\author{
Jonathan Kropko <jkropko@virginia.edu> and Jeffrey J. Harden <jharden@nd.edu>, based
on the code for the \code{\link[mediation]{mediate}} function in the \code{mediation} package
by Dustin Tingley, Teppei Yamamoto, Kentaro Hirose, Luke Keele, and Kosuke Imai.
}
