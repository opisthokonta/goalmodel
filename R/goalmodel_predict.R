# This script contain interfacte functions for making predictions based on a
# fitted goalmodel object.



#' Predict methods for goalmodel Fits
#'
#' Make predictions using a fitted goalmodel.
#'
#' These functions predict expected goals, the probabiltieis fo individual scorelines, or
#' 1x2 results and over/under probabilities.
#'
#' @param model_fit A goalmodel object.
#' @param team1 A character vector with team names, for which to make predictions of.
#' @param team2 A character vector with team names, for which to make predictions of.
#' @param x1 Additional covariates to be used for making predictions.
#' @param x2 Additional covariates to be used for making predictions.
#' @param ou Numeric, defining the over/under. Default is 2.5.
#' @param return_df Wether a data.frame should be returned.
#'
#' @export
predict_expg <- function(model_fit, team1, team2, x1=NULL, x2=NULL, return_df = FALSE){

  stopifnot(length(team1) == length(team2),
            all(team1 %in% model_fit$all_teams),
            all(team2 %in% model_fit$all_teams))

  team1 <- as.character(team1)
  team2 <- as.character(team2)

  ee <- lambda_pred(model_fit$parameters, team1, team2, x1, x2)

  if (return_df){
    out <- data.frame(team1 = team1, team2 = team2,
                      expg1 = ee$expg1, expg2 = ee$expg2,
                      stringsAsFactors = FALSE)
  } else {
    out <- ee
  }

  return(out)
}


#' @rdname predict_expg
#' @export
predict_goals <- predict_result <- function(model_fit, team1, team2,
                                            x1=NULL, x2=NULL){

  stopifnot(length(team1) == length(team2),
            all(team1 %in% model_fit$all_teams),
            all(team2 %in% model_fit$all_teams))

  ## predict the expected goals.
  expg <- predict_expg(model_fit, team1 = team1, team2 = team2,
                       x1 = x1, x2 = x2, return_df = FALSE)

  # find the upper limit of where to evaluate the probability function.
  upper_prob <- 0.999
  if (model_fit$model == 'poisson'){
    maxgoal <- stats::qpois(upper_prob, lambda=model_fit$maxgoal)
  }
  else if (model_fit$model == 'negbin'){
    maxgoal <- stats::qnbinom(upper_prob, mu=model_fit$maxgoal, size = 1 / model_fit$parameters$dispersion)
  }
  maxgoal <- max(10, maxgoal)

  res <- vector(mode = 'list', length = length(team1))

  if (model_fit$model == 'poisson' & 'dispersion' %in% names(model_fit$parameters)){
    warning('The model object has a dispersion parameter, but model is Poisson. The dispersion parameter will not have an effect.')
  }

  for (ii in 1:length(team1)){
    if (model_fit$model == 'poisson'){
      res_tmp <- stats::dpois(0:maxgoal, expg$expg1[ii]) %*% t(stats::dpois(0:maxgoal, expg$expg2[ii]))
    } else if (model_fit$model == 'negbin'){
      res_tmp <- stats::dnbinom(0:maxgoal, mu = expg$expg1[ii], size = 1 / model_fit$parameters$dispersion) %*%
        t(stats::dnbinom(0:maxgoal, mu = expg$expg2[ii], size = 1 / model_fit$parameters$dispersion))
    }

    # Dixon-Coles adjustemt.
    if (!is.null(model_fit$parameters$rho)){
      correctionmat <- matrix(tau(c(0,1,0,1), c(0,0,1,1),
                                  rep(expg$expg1[ii], 4),
                                  rep(expg$expg2[ii], 4), model_fit$parameters$rho), nrow=2)
      res_tmp[1:2, 1:2] <- res_tmp[1:2, 1:2] * correctionmat
    }


    res_tmp <- res_tmp / sum(res_tmp) # normalize to make sure probabilities sum to 1.

    if (any(res_tmp < 0)){
      warning(sprintf('predict_goals: negative proabilities in game %d', ii))
    }

    res[[ii]] <- res_tmp
  }

  return(res)

}


#' @rdname predict_expg
#' @export
predict_result <- function(model_fit, team1, team2,
                           x1=NULL, x2=NULL, return_df = FALSE){

  stopifnot(length(team1) == length(team2),
            all(team1 %in% model_fit$all_teams),
            all(team2 %in% model_fit$all_teams))

  ## Compute bivariate probability distribution of goals.
  dgoals <- predict_goals(model_fit, team1 = team1, team2 = team2,
                          x1 = x1, x2 = x2)

  probd <- numeric(length(team1))
  prob1 <- numeric(length(team1))
  prob2 <- numeric(length(team1))

  for (ii in 1:length(team1)){
    probd[ii] <- sum(diag(dgoals[[ii]]))
    prob1[ii] <- sum(dgoals[[ii]][lower.tri(dgoals[[ii]])])
    prob2[ii] <- 1 - (probd[ii] + prob1[ii])
  }

  if (return_df){
    out <- data.frame(team1 = team1, team2 = team2,
                      p1 = prob1, pd = probd, p2 = prob2,
                      stringsAsFactors = FALSE)

  } else {
    out <- matrix(c(prob1, probd, prob2), ncol=3, nrow=length(team1))
  }

  return(out)

}


#' @rdname predict_expg
#' @export
predict_ou <- function(model_fit, team1, team2,
                       x1=NULL, x2=NULL, ou=2.5, return_df = FALSE){

  stopifnot(length(team1) == length(team2),
            all(team1 %in% model_fit$all_teams),
            all(team2 %in% model_fit$all_teams))


  team1 <- as.character(team1)
  team2 <- as.character(team2)

  if (model_fit$model == 'poisson' & 'dispersion' %in% names(model_fit$parameters)){
    warning('The model object has a dispersion parameter, but model is Poisson. The dispersion parameter will not have an effect.')
  }

  ee <- lambda_pred(model_fit$parameters, team1, team2, x1, x2)

  if (model_fit$model == 'poisson'){
    exp_goal_tot <- ee$expg1 + ee$expg2
    prob_under <- stats::ppois(floor(ou), lambda = exp_goal_tot)
    prob_over <- 1 - prob_under
  } else if (model_fit$model == 'negbin'){
    exp_goal_tot <- ee$expg1 + ee$expg2
    prob_under <- stats::pnbinom(floor(ou), mu = exp_goal_tot,
                          size = 1 / model_fit$parameters$dispersion)
    prob_over <- 1 - prob_under
  }


  if (return_df){
    out <- data.frame(team1 = team1, team2 = team2,
                      prob_under = prob_under, prob_over = prob_over,
                      stringsAsFactors = FALSE)
  } else {
    names(prob_under) <- NULL
    names(prob_over) <- NULL
    out <- list(prob_under=prob_under, prob_over=prob_over)
  }

  return(out)

}

