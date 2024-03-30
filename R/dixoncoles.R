

#' The Dixon-Coles-Poisson Distribution
#'
#' Probability mass function for the Dixon-Coles-Poisson distribution.
#'
#' @param x1 non-negative integer.
#' @param x2 non-negative integer.
#' @param lambda1 vector of non-negative parameters of the Dixon-Coles-Poisson distribution.
#' @param lambda2 vector of non-negative parameters of the Dixon-Coles-Poisson distribution.
#' @param rho vector of parameters of the Dixon-Coles-Poisson distribution.
#'
#'
#' @section References:
#' \itemize{
#'  \item{Mark J.  Dixon & Stuart G. Coles (1997) Modelling Association Football Scores and Inefficiencies in the Football Betting Market }
#' }
#'
#' @export
dDCP <- function(x1, x2, lambda1, lambda2, rho=NULL){

  N <- max(length(x1), length(x2),
             length(lambda1), length(lambda2), length(rho))

  res <- numeric(N)

  for (ii in 0:(N-1)){

    x1_idx <- (ii %% length(x1)) + 1
    x2_idx <- (ii %% length(x2)) + 1
    l1_idx <- (ii %% length(lambda1)) + 1
    l2_idx <- (ii %% length(lambda2)) + 1
    rho_idx <- (ii %% length(rho)) + 1

    #browser()

    res_tmp <- stats::dpois(x = x1[x1_idx], lambda = lambda1[l1_idx]) *
      stats::dpois(x = x2[x2_idx], lambda = lambda2[l2_idx])

    tau <- NULL

    if (!is.null(rho[rho_idx])){
      if (x1[x1_idx] == 0 & x2[x2_idx] == 0){
        tau <-  1 - lambda1[l1_idx] * lambda2[l2_idx] * rho[rho_idx]
      } else if (x1[x1_idx] == 0 & x2[x2_idx] == 1){
        tau <- 1 + lambda1[l1_idx] * rho[rho_idx]
      } else if (x1[x1_idx] == 1 & x2[x2_idx] == 0){
        tau <- 1 + lambda2[l2_idx] * rho[rho_idx]
      } else if (x1[x1_idx] == 1 & x2[x2_idx] == 1){
        tau <- 1 - rho[rho_idx]
      }
    }


    if (is.null(tau)){
      res[ii+1] <- res_tmp
    } else {
      res[ii+1] <- res_tmp * tau
    }

  }

  return(res)

}




#' Estimate the Rho Parameter of the Dixon-Coles-Poisson Model
#'
#' Given the estimated goals (lambda1, lambda2) and observed goals (x1, x2), estimate the rho parameter of the
#' the  Dixon-Coles-Poisson model.
#'
#' @param x1 non-negative integer.
#' @param x2 non-negative integer.
#' @param lambda1 vector of non-negative parameters of the Dixon-Coles-Poisson distribution.
#' @param lambda2 vector of non-negative parameters of the Dixon-Coles-Poisson distribution.
#' @param lower the lower bound for rho
#' @param upper the upper bound for rho
#'
#' @section References:
#' \itemize{
#'  \item{Mark J.  Dixon & Stuart G. Coles (1997) Modelling Association Football Scores and Inefficiencies in the Football Betting Market }
#' }
#'
#' @export
rho.ml <- function(x1, x2, lambda1, lambda2, lower = -0.5, upper = 0.5){


  # Define negativ log-likelihood function for RHO.
  obj_func <- function(par, x1, x2, lambda1, lambda2){
    sum(log(dDCP(x1 = x1, x2 = x2, lambda1 = lambda1, lambda2 = lambda2, rho = par)))*-1
  }


  # Find the maximum likelihood.
  optim_res <- stats::optim(par = 0.0, fn = obj_func,
                            x1 = x1,
                            x2 = x2,
                            lambda1 = lambda1,
                            lambda2 = lambda2,
                            method='Brent', lower = lower, upper = upper)

  if (optim_res$convergence != 0){
    warning('Did not converge (optim). Parameter estimates are unreliable.')
  }


  return(optim_res$par)

}




