#' Bias-corrected and accelerated confidence intervals
#' 
#' This function is taken from the 'coxed' package version 0.3.3 (archived on
#' CRAN). It is included here without modification solely because the package
#' has been removed from CRAN. Original authorship and credit belong to the
#' developers of the 'coxed' package. Source:
#' https://cran.r-project.org/package=coxed (archived)
#' 
#' This function uses the method proposed by DiCiccio and Efron (1996)
#' to generate confidence intervals that produce more accurate coverage
#' rates when the distribution of bootstrap draws is non-normal.
#' This code is adapted from the \code{BC.CI()} function within the
#' \code{\link[mediation]{mediate}} function in the \code{mediation} package.
#'
#' @param theta a vector that contains draws of a quantity of interest using bootstrap samples.
#' The length of \code{theta} is equal to the number of iterations in the previously-run
#' bootstrap simulation.
#' @param conf.level the level of the desired confidence interval, as a proportion. Defaults to
#' .95 which returns the 95 percent confidence interval.
#'
#' @details \eqn{BC_a} confidence intervals are typically calculated using influence statistics
#' from jackknife simulations. For our purposes, however, running jackknife simulation in addition
#' to ordinary bootstrapping is too computationally expensive. This function follows the procedure
#' outlined by DiCiccio and Efron (1996, p. 201) to calculate the bias-correction and acceleration
#' parameters using only the draws from ordinary bootstrapping.
#' @return returns a vector of length 2 in which the first element is the lower bound and the
#' second element is the upper bound
#'
#' @author Jonathan Kropko <jkropko@@virginia.edu> and Jeffrey J. Harden <jharden@@nd.edu>, based
#' on the code for the \code{\link[mediation]{mediate}} function in the \code{mediation} package
#' by Dustin Tingley, Teppei Yamamoto, Kentaro Hirose, Luke Keele, and Kosuke Imai.
#' @references DiCiccio, T. J. and B. Efron. (1996). Bootstrap Confidence Intervals. \emph{Statistical Science}.
#' 11(3): 189–212. \url{https://doi.org/10.1214/ss/1032280214}
#' @importFrom stats qnorm quantile

bca <- function(theta, conf.level = .95){
  low <- (1 - conf.level)/2
  high <- 1 - low
  sims <- length(theta)
  z.inv <- length(theta[theta < mean(theta)])/sims
  z <- qnorm(z.inv)
  U <- (sims - 1) * (mean(theta, na.rm=TRUE) - theta)
  top <- sum(U^3)
  under <- 6 * (sum(U^2))^{3/2}
  a <- top / under
  lower.inv <-  pnorm(z + (z + qnorm(low))/(1 - a * (z + qnorm(low))))
  lower <- quantile(theta, lower.inv, names=FALSE)
  upper.inv <-  pnorm(z + (z + qnorm(high))/(1 - a * (z + qnorm(high))))
  upper <- quantile(theta, upper.inv, names=FALSE)
  return(c(lower, upper))
}

