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
#' This function computes weights that give less weight to games further back in time.
#'
#' The weigths w are computed using the formula
#'
#' w = exp(-xi*t)
#'
#' where xi is a tuning parameter that decides the amount of down-weighting,
#' and t is the number of days since currentDate. If any date in dates are after
#' currentDate, the corresponding weights are set to 0.
#'
#'
#' @param dates a vector of dates (of type Date).
#' @param xi A numeric with the time dumping factor. Ususally a value between 0.001 and 0.003.
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



# Internal function for computing the squared error for estimating
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

  # Convert to matrix
  if (!is.matrix(probabilities)){
    if (is.numeric(probabilities)){
      probabilities <- matrix(probabilities, nrow=1,
                                 dimnames = list(NULL, names(probabilities)))
    } else {
      probabilities <- as.matrix(probabilities)
    }


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



#' Estimate the expected goals from over/under probabilities.
#'
#' From a probability of goals scored being below a given number (like 2.5), compute
#' the expected number of goals scored, assuming an underlying Poisson distribution.
#'
#' @param probability Vector of probabilities for number of goals scored being less (or greater) than ou.
#' @param ou The limit the probability refers to. Default is 2.5.
#' @param under Logical indicating whether the probabilities are for 'under' (TRUE, default) or 'over' (FALSE).
#' @param upperlim Numeric giving the upper limit for expected goals the estimating procedure will consider.
#'
#'@return
#' A numeric vector with expected number of goals.
#'
#' @export
expg_from_ou <- function(probability, ou=2.5, under = TRUE, upperlim = 10){

  stopifnot(all(probability >= 0),
            all(probability <= 1),
            length(ou) == 1,
            ou > 0,
            is.logical(under),
            length(under) == 1)

  # If probabilities are given as 'over', convert them to 'under'.
  if (!under){
    probability <- 1 - probability
  }

  obj <- function(expg, prob, under){
    stats::ppois(floor(under), lambda=expg) - prob
  }

  expg <- numeric(length(probability))
  for (ii in 1:length(probability)){

    res <- stats::uniroot(f = obj,
                         interval = c(0.001, upperlim),
                         prob = probability[ii],
                         under = ou)

    expg[ii] <- res$root
  }

  return(expg)

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





#' Create league table
#'
#' Creates a league table with basic descriptive statistics and number of points.
#'
#' Points are calculated as 3 points for win, 1 for draw, and 0 for loss. Results
#' are sorted by points and ties will be resolved by 1) goal difference and 2) goals scored.
#' Further ties will not be resolved, and the tied teams will be ranked arbitrarily.
#'
#' Note that leagues and competitions will often have different rules for how teams
#' are ranked, especially for how ties are handled. The results provided by this functions
#' may therefore differ from official league rankings.
#'
#' Games with missing values in the number of goals scores will be ignored and will
#' therefore not contribute to the table.
#'
#' @param goals1 Numeric, non-negative integer. The number of goals scored by team 1.
#' @param goals2 Numeric, non-negative integer. The number of goals scored by team 2.
#' @param team1 Vector of team names.
#' @param team2 Vector of team names.
#'
#' #' @return
#'  A data.frame with one row for each team in the data.
#'
#' @export
league_table <- function(goals1, goals2, team1, team2){

  team1 <- as.character(team1)
  team2 <- as.character(team2)

  stopifnot(length(goals1) == length(goals2),
            length(goals2) == length(team1),
            length(team1) == length(team2),
            is.numeric(goals1),
            is.numeric(goals2))

  #Remove missing values
  mising_idx <- is.na(goals1) | is.na(goals2) | is.na(team1) | is.na(team2)
  goals1 <- goals1[!mising_idx]
  goals2 <- goals2[!mising_idx]
  team1 <- team1[!mising_idx]
  team2 <- team2[!mising_idx]

  all_teams <- sort(unique(c(unique(team1), unique(team2))), decreasing = FALSE)
  n_teams <- length(all_teams)
  ngames <- length(goals1)

  goaldiff <- goals1 - goals2


  # init result vectors
  games_played <- rep(0, n_teams)
  names(games_played) <- all_teams

  points <- rep(0, n_teams)
  names(points) <- all_teams

  goals_scored <- rep(0, n_teams)
  names(goals_scored) <- all_teams

  goals_against <- rep(0, n_teams)
  names(goals_against) <- all_teams

  wins <- rep(0, n_teams)
  names(wins) <- all_teams

  draws <- rep(0, n_teams)
  names(draws) <- all_teams

  loss <- rep(0, n_teams)
  names(loss) <- all_teams


  # Loop trough all games
  for (ii in 1:ngames){

    games_played[team1[ii]] <- games_played[team1[ii]] + 1
    games_played[team2[ii]] <- games_played[team2[ii]] + 1

    goals_scored[team1[ii]] <- goals_scored[team1[ii]] + goals1[ii]
    goals_scored[team2[ii]] <- goals_scored[team2[ii]] + goals2[ii]

    goals_against[team1[ii]] <- goals_against[team1[ii]] + goals2[ii]
    goals_against[team2[ii]] <- goals_against[team2[ii]] + goals1[ii]

    if (goaldiff[ii] > 0){
      wins[team1[ii]] <- wins[team1[ii]] + 1
      loss[team2[ii]] <- loss[team2[ii]] + 1

    } else if (goaldiff[ii] == 0){
      draws[team1[ii]] <- draws[team1[ii]] + 1
      draws[team2[ii]] <- draws[team2[ii]] + 1
    } else if (goaldiff[ii] < 0) {
      wins[team2[ii]] <- wins[team2[ii]] + 1
      loss[team1[ii]] <- loss[team1[ii]] + 1

    } else {
      stop('sdfd')
    }

  }

  # TODO: Implement head-2-head tiebreaker rule.

  points = wins*3 + draws*1
  gd <- goals_scored - goals_against

  res <- data.frame(Team = all_teams,
                    Played = games_played,
                    Won = wins,
                    Drawn = draws,
                    Lost = loss,
                    Goals_scored = goals_scored,
                    Goals_conceded = goals_against,
                    Goal_difference = gd,
                    Pts = points)


  res <- res[order(res$Pts, res$Goal_difference, res$Goals_scored, decreasing = TRUE),]

  row.names(res) <- NULL

  return(res)

}



#' Scoring rules to evaluate prediction accuracy
#'
#' This function provides scoring rules for evaluating prediction accuracy.
#'
#' Currently three scoring rules are available: The log score, Brier score, and
#' the Ranked Probability Score (RPS).
#'
#' The log score is just the negative logarithm of the probability of the observed outcome.
#'
#' For all three scoring functions, a lower score means a better predictions. They will attain a
#' lowest possible score of 0 if the observed outcome were predicted with a 100% probability.'
#' The RPS has an upper limit of 1, which indicates the worst possible
#' prediction. The Brier score has an upper limit of 2. The log score has no upper
#' limit, and will be infinite if the observed outcome were predicted with probability 0.
#'
#'
#' @param predictions A matrix or data frame with probabilities, with one column for each outcome, and one row for each prediction.
#' @param observed Numeric or character vector of the same length as the number of predictions. It must contain an indicator of the observed outcome,
#' either a column number or a column name.
#' @param score Character vector of the scoring functions to use. Currently 'log', 'brier', and 'rps' are available.
#'
#' @references
#' Wheatcroft, E. (2021) Evaluating probabilistic forecasts of football matches: the case against the ranked probability score.
#' https://doi.org/10.1515/jqas-2019-0089
#'
#' Constantinou, A. C., and N. E. Fenton (2012) Solving the Problem of Inadequate Scoring Rules for Assessing Probabilistic Football Forecast Models. https://doi.org/10.1515/1559-0410.1418
#'
#'
#' @export
score_predictions <- function(predictions, observed, score){


  scorefunctions <- c('log', 'brier', 'rps')
  stopifnot(is.character(score),
            length(score) >= 1)

  score <- tolower(score)

  if (!all(score %in% scorefunctions)){
    stop('score must be one of brier, log or rps.')
  }

  # Some useful quantities.
  ncat <- ncol(predictions)
  npred <- nrow(predictions)

  if (is.numeric(observed)){
    stopifnot(any(observed >= 1),
              any(observed <= ncat),
              length(observed) == npred)
  } else if (is.character(observed)){
    stopifnot(any(observed %in% colnames(predictions)))
  }

  if (is.character(observed)){
    observed <- match(observed, colnames(predictions))
  }

  # Output list.
  res <- list()

  # Expand observed vector to indicator matrix of the same
  # dimensions as the predictions matrix.
  obsmat <- matrix(0, ncol=ncat, nrow=npred)
  for (rr in 1:npred){
    obsmat[rr, observed[rr]] <- 1
  }


  # Logarithmic scoring rule
  if ('log' %in% score){
    log_scores <- numeric(npred)
    for (rr in 1:npred){
      log_scores[rr] <- -log(predictions[rr,observed[rr]])
    }

    res$log <- log_scores

  }


  if ('brier' %in% score){
    res$brier <- rowSums((predictions - obsmat)^2)
  }


  # Ranked Probability Score. (RPS).
  if ('rps' %in% score){
    rankprobscore <- numeric(npred)

    for (rr in 1:npred){
      cumulative <- 0
      for (i in 1:ncat){
        cumulative <- cumulative + (sum(predictions[rr,1:i]) - sum(obsmat[rr, 1:i]))^2
      }
      rankprobscore[rr] <- (1/(ncat-1))*cumulative
    }

    res$rps <- rankprobscore

  }

  return(res)

}





