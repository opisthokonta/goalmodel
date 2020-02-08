# This script contains some miscellaneous functions, not directly invovled
# in model fitting and making predictions.

#' Print a summary of a fitted goalmodel.
#'
#' @param object A fitted goalmodel object.
#' @param ... ignored.
#'
#' @export
summary.goalmodel <- function(object, ...){

  if (object$converged){
    cat(sprintf('Model sucsessfully fitted in %.2f seconds\n\n', object$est_time))
  } else {
    cat('Model failed to converge. Parameter estimates are unreliable.\n\n')
  }

  # Data summary
  cat(sprintf('%-25.25s % 5.0d \n', 'Number of matches', object$ngames))
  cat(sprintf('%-25.25s % 5.0d \n', 'Number of teams', length(object$all_teams)))

  cat('\n')

  if (object$model == 'poisson'){
    cat(sprintf('%-25.25s %s \n', 'Model', 'Poisson'))
  } else if (object$model == 'negbin'){
    cat(sprintf('%-25.25s %s \n', 'Model', 'Neg. Binom.'))
  } else if (object$model == 'gaussian'){
    cat(sprintf('%-25.25s %s \n', 'Model', 'Gaussian'))
  } else if (object$model == 'cmp'){
    cat(sprintf('%-25.25s %s \n', 'Model', 'CMP'))
  }

  cat('\n')

  # fit indices
  cat(sprintf('%-25.25s % 2.2f \n', 'Log Likelihood', object$loglikelihood))
  cat(sprintf('%-25.25s % 2.2f \n', 'AIC', object$aic))
  cat(sprintf('%-25.25s % 2.2f \n', 'R-squared', object$r_squared))
  cat(sprintf('%-25.25s % 5.1d \n', 'Parameters (estimated)', object$npar_est))
  cat(sprintf('%-25.25s % 5.1d \n', 'Parameters (fixed)', object$npar_fixed))

  cat('\n')

  # Attack and defense parameters
  cat(sprintf('%-25s %-3s   %-3s\n', 'Team', 'Attack', 'Defense'))
  for (ii in 1:length(object$all_teams)){
    ct <- object$all_teams[ii]
    cat(sprintf('%-25.25s % 2.2f    % 2.2f \n',
                ct,
                object$parameters$attack[ct],
                object$parameters$defense[ct]))
  }
  cat('-------\n')
  cat(sprintf('%-25.25s % 2.2f \n', 'Intercept', object$parameters$intercept))

  if ('hfa' %in% names(object$parameters)){
    cat(sprintf('%-25.25s % 2.2f \n', 'Home field advantage', object$parameters$hfa))
  }

  if ('rho' %in% names(object$parameters)){
    cat(sprintf('%-25.25s % 2.2f \n', 'Dixon-Coles adj. (rho)', object$parameters$rho))
  }

  if ('dispersion' %in% names(object$parameters) & object$model == 'negbin'){
    cat(sprintf('%-25.25s % 2.2f \n', 'Dispersion (Neg. Binom.)', object$parameters$dispersion))
  }

  if ('dispersion' %in% names(object$parameters) & object$model == 'cmp'){
    cat(sprintf('%-25.25s % 2.2f \n', 'Dispersion (CMP)', object$parameters$dispersion))
  }


  if ('sigma' %in% names(object$parameters)){
    cat(sprintf('%-25.25s % 2.2f \n', 'Sigma (Gaussian)', object$parameters$sigma))
  }

  if ('gamma' %in% names(object$parameters)){
    cat(sprintf('%-25.25s % 2.2f \n', 'Rue-Salvesen adj. (gamma)', object$parameters$gamma))
  }

  if ('beta' %in% names(object$parameters)){
    for (ii in 1:length(object$parameters$beta)){
      cat(sprintf('%-25.25s % 2.2f \n', names(object$parameters$beta)[ii], object$parameters$beta[ii]))
    }
  }

}


#' Compute Dixon-Coles weights
#'
#' @param dates a vector of dates (a type Date).
#' @param xi A numeric with the time dumping factor. Ususally a value buetween 0.001 and 0.003.
#' @param currentDate The date which to count backwards from. Default to the latest date in dates.
#'
#' @export
weights_dc <- function(dates, xi=0, currentDate=NULL){
  stopifnot(xi >= 0,
            length(xi) == 1)
  dates <- as.Date(dates)

  if (is.null(currentDate)){
    currentDate <- max(dates)
  } else {
    currentDate <- as.Date(currentDate)
  }

  datediffs <- dates - as.Date(currentDate)
  datediffs <- as.numeric(datediffs *-1)
  w <- exp(-1*xi*datediffs)
  w[datediffs < 0] <- 0 #Future dates should have zero weights
  return(w)
}

