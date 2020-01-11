

#' The Conway-Maxwell-Poisson Distribution
#'
#' Functions to compute the probability (dCMP) and cumulative probability (pCMP)
#' for the Conway-Maxwell-Poisson distribution, with rate parameter lambda and
#' dispersion parameter upsilon. There is also a function for computing the
#' expectation (eCMP) and for allowing a re-parameterization by the mean
#' paramater mu (lambdaCMP).
#'
#' When upsilon is greater than 1, the variance is lower than the expectation,
#' while it is larger than expectation when upsilon is in the 0-1 range. The
#' variance equals expectation when upsilon = 1, and then the same as the
#' Poisson distribution.
#'
#' There is no general closed form for the expectation for the CMP
#' distribution, and so it must be approximated. Two methods are available
#' for this. The 'fast' method uses the following approximation in Shmueli et
#' al (2005) for the relationship between the parameters and the expectation:
#'
#' mu = E(x) ~ lambda^(1/upsilon) - ((upsilon - 1) / (2*upsilon))
#'
#' The 'sum' method relies on using a truncated sum from 0 to K, where K is
#' determined using the error argument, as in Shmueli et al (2005). For the
#' expectation the sum(x*p(x)) from x = (0, Inf) is approximated. For lambda,
#' the equation sum((mu-x) * p(x)) = 0 is solved (see Huang 2017), with the
#' left hand side sum is approximated.
#'
#' @param x vector of non-negative integers.
#' @param lambda vector of non-negative rate parameters.
#' @param upsilon vector of non-negative dispersion parameters.
#' @param mu vector of non-negative mean parameters.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are P[X ≤ x], otherwise, P[X > x].
#' @param method character; either 'fast' or 'sum'. See 'details'.
#' @param error numeric. Upper bound for the error in computing the normalizing constant.
#'
#' @section References:
#' \itemize{
#'  \item{Shmueli et al (2005) A useful distribution for fitting discrete data: revival of the Conway–Maxwell–Poisson distribution }
#'  \item{Huang (2017) Mean-parametrized Conway–Maxwell–Poisson regression models for dispersed counts}
#' }
#' @name CMP
NULL


# Compute the normalizing constant for the Conway-Mawell-Poisson.
CMP_normalizing_constant <- function(lambda, upsilon, error=0.01){

  stopifnot(any(!(lambda >= 1 & upsilon == 0)))

  # Upper limit of the sum.
  ul <- pmax(50, ceiling((lambda / error)^(1/upsilon)))

  # The Normalizing constant.
  cc <- mapply(FUN=function(lambda, upsilon, ul){
    # The terms of the constant are computed on the log scale,
    # before they are exponentiated and summed.
    sum(exp((0:ul * log(lambda)) - (upsilon * lfactorial(0:ul))))
  }, lambda, upsilon, ul)

  return(cc)

}



#' @rdname CMP
#' @export
dCMP <- function(x, lambda, upsilon, log=FALSE, error=0.01){
  # http://en.wikipedia.org/wiki/Conway%E2%80%93Maxwell%E2%80%93Poisson_distribution
  # reparameteriseing
  # http://onlinelibrary.wiley.com/doi/10.1111/j.1539-6924.2008.01014.x/abstract

  stopifnot(any(!(lambda >= 1 & upsilon == 0)),
            is.logical(log))

  # the Normalizing constant.
  cc <- CMP_normalizing_constant(lambda, upsilon, error=0.01)

  log_prob <- (x * log(lambda)) - (upsilon * lfactorial(x)) - log(cc)

  if (log == FALSE){
    res <- exp(log_prob)
  } else {
    res <- log_prob
  }

  return(res)

}

#' @rdname CMP
pCMP <- function(x, lambda, upsilon, error=0.01, lower.tail=TRUE){

  stopifnot(any(!(lambda >= 1 & upsilon == 0)),
            is.logical(lower.tail))

  # the Normalizing constant.
  cc <- CMP_normalizing_constant(lambda, upsilon, error=error)

  pp <- mapply(FUN=function(x, lambda, upsilon, cc){
    sum(exp(((0:x * log(lambda)) - (upsilon * lfactorial(0:x))) - log(cc)))
  }, x, lambda, upsilon, cc)

  if (lower.tail == FALSE){
    pp <- 1 - pp
  }

  return(pp)
}

# sum(dCMP(x=0:6, lambda=4.4, upsilon = 1.2))
# sum(pCMP(x=6, lambda=4.4, upsilon = 1.2))

#' @rdname CMP
#' @export
eCMP <- function(lambda, upsilon, method='sum', error=0.01){

  stopifnot(any(!(lambda >= 1 & upsilon == 0)))

  if (method == 'fast'){
    ee <- lambda^(1/upsilon) - ((upsilon - 1) / (2*upsilon))
  } else if (method == 'sum') {

    # Upper limit for computing the expectation
    # the same as for computing the normalization constant.
    ul <- ceiling((lambda / error)^(1/upsilon))

    # the Normalizing constant.
    cc <- CMP_normalizing_constant(lambda, upsilon, error=error)

    # The expectation.
    ee <- mapply(FUN=function(lambda, upsilon, cc, ul){
      sum(exp(((0:ul * log(lambda))) - (upsilon * lfactorial(0:ul)) - log(cc)) * 0:ul)
    }, lambda, upsilon, cc, ul)

  }

  return(ee)

}


# The condition (eq. 2.2) in Huang (2017), for converting
# the mu and upsilon parameters to lambda.
lambda_cond <- function(lambda, mu, upsilon, ul=100){
  sum(exp(((0:ul * log(lambda))) - (upsilon * lfactorial(0:ul))) * ((0:ul) - mu))
}

# Approximation of lambda, based on the
# aprroximation for expectation given in Shmueli et al (2005)
lambda_approx <- function(mu, upsilon){
  (mu + ((upsilon - 1) / (2*upsilon))) ^ (upsilon)
}


#' @rdname CMP
#' @export
lambdaCMP <- function(mu, upsilon, method='sum', error = 0.01){

  lambdas <- lambda_approx(mu=mu, upsilon = upsilon)

  if (method == 'fast'){
    res <- lambdas
  } else if (method == 'sum'){

    stopifnot(length(upsilon) == 1)

    ul <- pmax(50, ceiling((lambdas / error)^(1/upsilon)))

    for (ii in 1:length(lambdas)){

      # use the lambda approximation to find search intervall.
      lambda_interval <- pmax(0.001, lambdas[ii] + c(-5, 5))

      uniroot_res <- stats::uniroot(f=lambda_cond,
                             interval = lambda_interval,
                             extendInt = 'upX',
                             mu=mu[ii], upsilon=upsilon, ul=ul[ii])

      lambdas[ii] <- uniroot_res$root
    }

    res <- lambdas

  }

  return(res)

}


