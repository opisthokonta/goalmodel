# This script contain interfacte functions for making predictions based on a
# fitted goalmodel object.



#' Predict Methods for goalmodel Fits
#'
#' Make predictions using a fitted goalmodel.
#'
#' These functions predict expected goals, the probabiltieis for individual scorelines, or
#' 1x2 results and over/under probabilities.
#'
#' @param model_fit A goalmodel object.
#' @param team1 A character vector with team names, for which to make predictions of.
#' @param team2 A character vector with team names, for which to make predictions of.
#' @param x1 Additional covariates to be used for making predictions.
#' @param x2 Additional covariates to be used for making predictions.
#' @param ou Numeric, defining the over/under. Default is 2.5.
#' @param lwrx Numeric. The lowest upper limit for the number of goals to compute probabilities for.
#' @param return_df Whether a data.frame should be returned.
#'
#' @export
predict_expg <- function(model_fit, team1, team2, x1=NULL, x2=NULL, return_df = FALSE){

  stopifnot(length(team1) == length(team2))

  team1 <- as.character(team1)
  team2 <- as.character(team2)

  ee <- lambda_pred(model_fit$parameters, team1, team2, x1, x2)

  if (any(is.na(ee$expg1) | is.na(ee$expg2))){
    warning('Could not make predictions in some instances.')
  }

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
predict_goals <- function(model_fit, team1, team2, x1=NULL, x2=NULL,
                          return_df = FALSE, lwrx=NULL){

  stopifnot(length(team1) == length(team2),
            is.logical(return_df))


  if (is.null(lwrx)){
    lwrx <- 25
  }

  if (model_fit$model %in% c('gaussian')){
    warning('Model is Gaussian. Predictions are made using a Poisson model based on the expected goals from the Gaussian fit.')
  }

  ## predict the expected goals.
  expg <- predict_expg(model_fit, team1 = team1, team2 = team2,
                       x1 = x1, x2 = x2, return_df = FALSE)

  # find the upper limit of where to evaluate the probability function.
  upper_prob <- 0.999
  if (model_fit$model %in% c('poisson', 'gaussian', 'cmp')){
    # TODO: This is probably not optimal if CMP is over-dispersed.
    maxgoal <- stats::qpois(upper_prob, lambda=model_fit$maxgoal)
  } else if (model_fit$model == 'negbin'){
    maxgoal <- stats::qnbinom(upper_prob, mu=model_fit$maxgoal, size = 1 / model_fit$parameters$dispersion)
  }
  maxgoal <- max(lwrx, maxgoal)

  if (model_fit$model == 'poisson' & 'dispersion' %in% names(model_fit$parameters)){
    warning('The model object has a dispersion parameter, but model is Poisson. The dispersion parameter will not have an effect.')
  }

  if (return_df){

    res <- data.frame(team1 = rep(team1, each=(maxgoal+1)^2),
                      team2 = rep(team2, each=(maxgoal+1)^2),
                      goals1 = rep(0:maxgoal, maxgoal+1   ),
                      goals2 = rep(0:maxgoal, each=maxgoal+1 ),
                      stringsAsFactors = FALSE)

    expg1_long <- expg$expg1[res$team1]
    expg2_long <- expg$expg2[res$team2]


    if (model_fit$model %in% c('poisson', 'gaussian')){
      res$probability <- dpois(res$goals1, expg1_long) * dpois(res$goals2, expg2_long)
    } else if (model_fit$model == 'negbin'){
      res$probability <- stats::dnbinom(res$goals1, mu = expg1_long, size = 1 / model_fit$parameters$dispersion) *
        stats::dnbinom(res$goals2, mu = expg2_long, size = 1 / model_fit$parameters$dispersion)
    } else if (model_fit$model == 'cmp'){

      ll1 <- lambdaCMP(mu = expg$expg1, upsilon = model_fit$parameters$dispersion, method = 'fast')
      ll2 <- lambdaCMP(mu = expg$expg2, upsilon = model_fit$parameters$dispersion, method = 'fast')

      names(ll1) <- names(expg$expg1)
      names(ll2) <- names(expg$expg2)

      res$probability <- dCMP(res$goals1, lambda = ll1[res$team1], upsilon = model_fit$parameters$dispersion) *
        dCMP(res$goals2, lambda = ll2[res$team2], upsilon = model_fit$parameters$dispersion)
    }

    # Dixon-Coles adjustemt.
    if (!is.null(model_fit$parameters$rho)){
      res$probability <- tau(goals1 = res$goals1, goals2 = res$goals2,
                            lambda1 = expg1_long, lambda2 = expg2_long,
                            rho = model_fit$parameters$rho) * res$probability
    }


  } else {

    # Return list of matrices.
    res <- vector(mode = 'list', length = length(team1))

    for (ii in 1:length(team1)){

      if (is.na(expg$expg1[ii]) | is.na(expg$expg2[ii])){
        res[[ii]] <- matrix(NA, ncol=maxgoal+1, nrow=maxgoal+1)
        next
      }

      if (model_fit$model %in% c('poisson', 'gaussian')){
        res_tmp <- stats::dpois(0:maxgoal, expg$expg1[ii]) %*% t(stats::dpois(0:maxgoal, expg$expg2[ii]))
      } else if (model_fit$model == 'negbin'){
        res_tmp <- stats::dnbinom(0:maxgoal, mu = expg$expg1[ii], size = 1 / model_fit$parameters$dispersion) %*%
          t(stats::dnbinom(0:maxgoal, mu = expg$expg2[ii], size = 1 / model_fit$parameters$dispersion))
      } else if (model_fit$model == 'cmp'){
          # Convert the expected goals in to CMP-lambda.
          ll1 <- lambdaCMP(mu = expg$expg1[ii], upsilon = model_fit$parameters$dispersion, method = 'fast')
          ll2 <- lambdaCMP(mu = expg$expg2[ii], upsilon = model_fit$parameters$dispersion, method = 'fast')

          res_tmp <- dCMP(0:maxgoal, lambda = ll1, upsilon = model_fit$parameters$dispersion) %*%
          t(dCMP(0:maxgoal, lambda = ll2, upsilon = model_fit$parameters$dispersion))
      }

      # Dixon-Coles adjustemt.
      if (!is.null(model_fit$parameters$rho)){
        correctionmat <- matrix(tau(c(0,1,0,1), c(0,0,1,1),
                                    rep(expg$expg1[ii], 4),
                                    rep(expg$expg2[ii], 4), model_fit$parameters$rho), nrow=2)
        res_tmp[1:2, 1:2] <- res_tmp[1:2, 1:2] * correctionmat
      }

      # normalize to make sure probabilities sum to 1.
      res_tmp <- res_tmp / sum(res_tmp)

      if (any(res_tmp < 0)){
        warning(sprintf('predict_goals: negative proabilities in game %d', ii))
      }

      res[[ii]] <- res_tmp
    }
  }

  return(res)

}


#' @rdname predict_expg
#' @export
predict_result <- function(model_fit, team1, team2,
                           x1=NULL, x2=NULL, return_df = FALSE){

  stopifnot(length(team1) == length(team2))

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

  stopifnot(length(team1) == length(team2))

  if (model_fit$model %in% c('gaussian')){
    warning('Model is Gaussian Predictions are made using a Poisson model based on the expected goals from the Gaussian fit.')
  }

  team1 <- as.character(team1)
  team2 <- as.character(team2)

  if (model_fit$model == 'poisson' & 'dispersion' %in% names(model_fit$parameters)){
    warning('The model object has a dispersion parameter, but model is Poisson. The dispersion parameter will not have an effect.')
  }

  ee <- lambda_pred(model_fit$parameters, team1, team2, x1, x2)
  exp_goal_tot <- ee$expg1 + ee$expg2

  if (model_fit$model %in% c('poisson', 'gaussian')){
    prob_under <- stats::ppois(floor(ou), lambda = exp_goal_tot)
  } else if (model_fit$model == 'negbin'){
    prob_under <- stats::pnbinom(floor(ou), mu = exp_goal_tot,
                          size = 1 / model_fit$parameters$dispersion)
  } else if (model_fit$model == 'cmp'){
    # Convert expected goals to CMP-lambda
    lltot <- lambdaCMP(mu = exp_goal_tot, upsilon = model_fit$parameters$dispersion,
                       method = 'fast')
    prob_under <- pCMP(floor(ou), lambda = lltot, upsilon = model_fit$parameters$dispersion)
  }

  prob_over <- 1 - prob_under

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

