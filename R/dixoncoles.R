

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

