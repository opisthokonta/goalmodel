# This script contains some miscellaneous functions, not directly invovled
# in model fitting and making predictions.

#' Print a summary of a fitted goalmodel.
#'
#' @param object A fitted goalmodel object.
#' @param ... ignored.
#'
#' @export
summary.goalmodel <- function(object, ...){

  if (length(object$warnings) == 0){
    cat(sprintf('Model sucsessfully fitted in %.2f seconds\n\n', object$est_time))
  } else {
    cat(sprintf('Model fitted with warnings (%d). Results may be unreliable.\n\n', length(object$warnings)))
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
#' @param dates a vector of dates (of type Date).
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



# Internal function for computnig the squared error for estimating
# the expected goals from outcome probabilities.
expg_prob_sq_error <- function(pars, trgt_probs, rho, uprx){

  pars <- exp(pars) # trick to avoid negaive lambda parameters.
  hda_probs <- numeric(3)
  probmat <- stats::dpois(0:uprx, lambda=pars[1]) %o% stats::dpois(0:uprx, lambda=pars[2])

  ## DC adjustment
  if (rho != 0){
    correctionmat <- matrix(tau(c(0,1,0,1), c(0,0,1,1),
                                rep(pars[1], 4),
                                rep(pars[2], 4), rho), nrow=2)
    probmat[1:2, 1:2] <- probmat[1:2, 1:2] * correctionmat
  }

  hda_probs[2] <- sum(diag(probmat))
  hda_probs[1] <- sum(probmat[lower.tri(probmat)])
  hda_probs[3] <- 1 - sum(hda_probs[1:2])

  sum((hda_probs - trgt_probs)^2)

}

#' Estimate the expected goals from win-draw-lose probabilities.
#'
#' This function converts outcome probabilities into expected goals,
#' assuming an underlying Poisson distribution, or a Dixon-Coles-Poisson
#' distribution with known dependence paramater rho.
#'
#' @param probabilities A 3-column matrix of win-draw-lose probabilities.
#' @param rho numeric. Value for the Dixon-Coles adjustment parameter. Default is 0, which is the same as no adjustment.
#' @param uprx numeric. The upper limit for evaluating the poisson distribution.
#'
#' @return A list with two elements. The first, expg, is a two-column matrix of
#' expected goals. The second, sq_error, is a numeric vector indicating the how well
#' the expected goals matches the probabilities using the poisson distribution.
#'
#'
#' @export
expg_from_probabilities <- function(probabilities, rho=0, uprx=75){

  if (!is.matrix(probabilities)){
    probabilities <- matrix(probabilities, nrow=1,
                            dimnames = list(NULL, names(probabilities)))
  }

  stopifnot(ncol(probabilities) == 3,
            all(abs(rowSums(probabilities) - 1) < 0.0001),
            uprx >= 1,
            length(uprx) == 1,
            length(rho) == 1)

  expg <- matrix(ncol=2, nrow=nrow(probabilities))
  sq_errors <- numeric(nrow(probabilities))

  for (ii in 1:nrow(probabilities)){

    optim_res <- stats::optim(c(0,0), fn=expg_prob_sq_error,
                       trgt_prob=probabilities[ii,],
                       rho = rho, uprx = uprx)

    expg[ii,] <- exp(optim_res$par)
    sq_errors[ii] <- optim_res$value

  }

  out <- list(expg = expg, sq_errors=sq_errors)

  return(out)
}



#' Time since last match
#'
#' This function calculates the number of days since the two teams
#' played their respective previous matches.
#'
#' @param team1 Vector of team names.
#' @param team2 Vector of team names.
#' @param dates a vector of dates (of type Date).
#' @param first_val the value used for a team's first match in the data, when there
#' are no preceding matches. Default is NA.
#'
#' @return A two-column matrix with the number of days since last time
#' each team played.
#'
#' @seealso \code{\link{matches_last_xdays}}
#'
#' @export
days_since_last_match <- function(team1, team2, dates, first_val = NA){

  stopifnot(length(team1) == length(team2),
            length(team2) == length(dates),
            length(team1) >= 1,
            length(first_val) == 1)

  dates <- as.Date(dates)
  ngames <- length(team1)
  all_teams <- sort(unique(c(unique(team1), unique(team2))), decreasing = FALSE)

  res <- matrix(NA, ncol=2, nrow=ngames)

  for (ii in 1:length(all_teams)){

    team_idx1 <- team1 == all_teams[ii]
    team_idx2 <- team2 == all_teams[ii]

    tidx <- as.numeric(team_idx1)
    tidx[team_idx2] <- 2

    team_idx <- which(team_idx1 | team_idx2)

    tdates <- dates[team_idx]

    for (tt in 1:length(tdates)){
      if (tdates[tt] == min(tdates)){
        tmp_val <- first_val
      } else {
        tmp_val <- tdates[tt] - max(tdates[tdates < tdates[tt]])
      }

      res[team_idx[tt], tidx[team_idx[tt]]] <- tmp_val

    }
  }

  return(res)

}



#' Number of matches played in the preceding period
#'
#' This function calculates the number of matches the two teams
#' have played the last x days.
#'
#' @param team1 Vector of team names.
#' @param team2 Vector of team names.
#' @param dates a vector of dates (of type Date).
#' @param days_since The number of days back in time to count matches.
#' @param first_val the value used for a team's first match in the data, when there
#' are no preceding matches. Default is NA.
#'
#' @return A two-column matrix with the number of matches played in the preceding
#' period of time, as given by the days_since argument.
#'
#' @seealso \code{\link{days_since_last_match}}
#'
#' @export
matches_last_xdays <- function(team1, team2, dates, days_since=30, first_val = NA){

  stopifnot(length(team1) == length(team2),
            length(team2) == length(dates),
            length(team1) >= 1,
            length(days_since) == 1,
            is.numeric(days_since))

  dates <- as.Date(dates)
  ngames <- length(team1)
  all_teams <- sort(unique(c(unique(team1), unique(team2))), decreasing = FALSE)

  res <- matrix(NA, ncol=2, nrow=ngames)

  for (ii in 1:length(all_teams)){

    team_idx1 <- team1 == all_teams[ii]
    team_idx2 <- team2 == all_teams[ii]

    tidx <- as.numeric(team_idx1)
    tidx[team_idx2] <- 2

    team_idx <- which(team_idx1 | team_idx2)

    tdates <- dates[team_idx]

    for (tt in 1:length(tdates)){
      if(tdates[tt] == min(tdates)){
        res[team_idx[tt], tidx[team_idx[tt]]] <- first_val
      } else {
        tmp_val <- pmax((tdates[tt] - tdates),0)
        res[team_idx[tt], tidx[team_idx[tt]]] <- sum(tmp_val <= days_since & tmp_val > 0)
      }

    }
  }

  return(res)

}
