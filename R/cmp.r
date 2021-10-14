

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
#' variance equals expectation when upsilon = 1, and then the distribution is
#' the same as the Poisson distribution.
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
#' @param p vector of quantiles.
#' @param q vector of probabilities.
#' @param lambda vector of non-negative rate parameters.
#' @param upsilon vector of non-negative dispersion parameters.
#' @param mu vector of non-negative mean parameters.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param lower_tail logical; if TRUE (default), probabilities are P[X ≤ x], otherwise, P[X > x].
#' @param method character; either 'fast' or 'sum'. See 'details'.
#' @param error numeric. Upper bound for the error in computing the normalizing constant.
#'
#' @section References:
#' \itemize{
#'  \item{Shmueli et al (2005) A useful distribution for fitting discrete data: revival of the Conway–Maxwell–Poisson distribution }
#'  \item{Huang (2017) Mean-parametrized Conway–Maxwell–Poisson regression models for dispersed counts}
#' }
#'
#' @seealso
#'\code{\link{upsilon.ml}} for a function for estimating the dispersion parameter.

#' @name CMP
NULL


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


#' Estimate upsilon of the Conway-Maxwell-Poisson Distribution
#'
#' Given the estimated rate (lambda) or mean (mu) vector, estimate dispersion paramter (uspilon) of
#' the Conway-Maxwell-Poisson Distribution.
#'
#' @param x vector of non-negative integers.
#' @param parameters vector of non-negative rate parameters (lambda) or mean paramters (mu).
#' @param param_type string indicating whether the vector given to the parameters
#' argument is lambda (default) or mu.
#' @param method character; either 'fast' or 'sum'. Passed on to the lambdaCMP function
#' if param_type = 'mu'.
#' @param weights Numeric vector of weigths that determine the influence of each data point on the final parameter estimate.
#' @param lower the lower bound for upsilon
#' @param upper the upper bound for upsilon
#'
#'@seealso
#'\code{\link{CMP}} for more functions and details regarding the Conway-Maxwell-Poisson Distribution.
#'
#' @export
upsilon.ml <- function(x, parameters, param_type = 'lambda', method = 'sum',
                       weights = NULL,
                       lower=0.7, upper=4){

  stopifnot(param_type %in% c('lambda', 'mu'))

  if (!is.null(weights)){
    stopifnot(is.numeric(weights),
              length(x)==length(weights),
              all(weights >= 0),
              all(!is.na(weights)),
              !all(weights == 0))
  }

  # The negative log-likelihood
  obj_func <- function(par, x, params, weights=NULL, is_lambda = TRUE, lmethod='sum'){

    if (is_lambda == FALSE){
      params <- lambdaCMP(mu = params, upsilon = par, method=lmethod)
    }

    if (is.null(weights)){
      sum(dCMP(x, lambda = params, upsilon = par, log=TRUE) * -1)
    } else {
      sum(weights*dCMP(x, lambda = params, upsilon = par, log=TRUE) * -1)
    }
  }

  is_lambda <- param_type == 'lambda'

  upsilon_init <- 1
  optim_res <- stats::optim(upsilon_init, fn=obj_func,
                             x=x, params=parameters,
                             weights = weights,
                             is_lambda = is_lambda,
                             lmethod=method,
                             method='Brent', lower=lower, upper=upper)

  converged <- optim_res$convergence == 0

  if (!converged){
    warning('Did not converge (optim). Parameter estimates are unreliable.')
  }

  return(optim_res$par)
}
