# This script contain interfacte functions for making predictions based on a
# fitted goalmodel object.



#' Predict Methods for goalmodel Fits
#'
#' Make predictions using a fitted goalmodel.
#'
#' These functions predict expected goals, the probabilities for individual scorelines,
#' 1x2 results, both teams to score, and total over/under probabilities.
#'
#' @param model_fit A goalmodel object.
#' @param team1 A character vector with team names, for which to make predictions of.
#' @param team2 A character vector with team names, for which to make predictions of.
#' @param x1 Additional covariates to be used for making predictions.
#' @param x2 Additional covariates to be used for making predictions.
#' @param ou Numeric, defining the over/under. Default is 2.5.
#' @param maxgoal Numeric, the upper limit of how many goals to make predictions for. If not set, a reasonable upper limit will be found based on the data used to fit the model.
#' @param lwrx Numeric. The lowest upper limit for the number of goals to compute probabilities for, if maxgoal is not given.
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
                      stringsAsFactors = FALSE, row.names = NULL)
  } else {
    out <- ee
  }


  return(out)
}



#' @rdname predict_expg
#' @export
predict_goals <- function(model_fit, team1, team2, x1=NULL, x2=NULL,
                          return_df = FALSE, maxgoal = NULL, lwrx=NULL){

  stopifnot(length(team1) == length(team2),
            is.logical(return_df))

  team1 <- as.character(team1)
  team2 <- as.character(team2)


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
  if (is.null(maxgoal)){
    upper_prob <- 0.999
    if (model_fit$model %in% c('poisson', 'gaussian', 'cmp')){
      # TODO: This is probably not optimal if CMP is over-dispersed.
      maxgoal <- stats::qpois(upper_prob, lambda=model_fit$maxgoal)
    } else if (model_fit$model == 'negbin'){
      maxgoal <- stats::qnbinom(upper_prob, mu=model_fit$maxgoal, size = 1 / model_fit$parameters$dispersion)
    }
    maxgoal <- max(lwrx, maxgoal)
  } else {
    stopifnot(is.numeric(maxgoal),
              length(maxgoal) == 1,
              maxgoal >= 0)
    maxgoal <- ceiling(maxgoal)
  }


  if (model_fit$model == 'poisson' & 'dispersion' %in% names(model_fit$parameters)){
    warning('The model object has a dispersion parameter, but model is Poisson. The dispersion parameter will not have an effect.')
  }

  if (return_df){

    res <- data.frame(team1 = rep(team1, each=(maxgoal+1)^2),
                      team2 = rep(team2, each=(maxgoal+1)^2),
                      goals1 = rep(0:maxgoal, maxgoal+1),
                      goals2 = rep(0:maxgoal, each=maxgoal+1),
                      stringsAsFactors = FALSE)

    expg1_long <- expg$expg1[res$team1]
    expg2_long <- expg$expg2[res$team2]


    if (model_fit$model %in% c('poisson', 'gaussian')){
      res$probability <- stats::dpois(res$goals1, expg1_long) * stats::dpois(res$goals2, expg2_long)
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

    # Dixon-Coles adjustment
    if (!is.null(model_fit$parameters$rho)){
      res$probability <- tau(goals1 = res$goals1, goals2 = res$goals2,
                            lambda1 = expg1_long, lambda2 = expg2_long,
                            rho = model_fit$parameters$rho) * res$probability
    }

    # 0-0 Hurdle adjustment
    # if (!is.null(model_fit$parameters$hurdle)){
    #
    # }


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


# TODO: Make predict_result() use this function.

#' Compute 1x2 probabilities from expected goals.
#'
#' @param expg1 Non-negative numeric. The expected number of goals.
#' @param expg2 Non-negative numeric. The expected number of goals.
#' @param model String indicating whether the goals follow a 'poisson' model (default), a Negative Binomial ('negbin'), or Conway-Maxwell-Poisson ('cmp') model.
#' @param dispersion Non-negative numeric. The dispersion parameter in the Negative Binomial model or the Conway-Maxwell-Poisson model.
#' @param rho Numeric. The Dixon-Coles adjustment.
#' @param uprx Numeric. The upper limit for evaluating the underlying distributions.
#'
#' @return
#' A matrix with 3 columns with one row for each pair of expected goals.
#'
#'
#' @export
p1x2 <- function(expg1, expg2, model = 'poisson', dispersion=NULL, rho=NULL, uprx=25){

  stopifnot(length(expg1) == length(expg2),
            all(expg1 >= 0),
            all(expg2 >= 0),
            length(model) >= 1,
            is.numeric(expg1), is.numeric(expg2),
            model %in% c('poisson', 'negbin', 'cmp'))


  nn <- length(expg1)

  if (model %in% c('negbin', 'cmp') & is.null(dispersion)){
    stop('Dispersion parameter not provided.')
  }

  if (!is.null(dispersion)){
    stopifnot(length(dispersion) == 1 | length(dispersion) == nn,
              is.numeric(dispersion),
              all(dispersion > 0))

    if (length(dispersion) == 1){
      dispersion <- rep(dispersion, nn)
    }

    if (model == 'poisson'){
      warning('Dispersion parameter will be ignored for model = "poisson".')
    }

  }

  if (!is.null(rho)){
    stopifnot(length(rho) == 1 | length(rho) == nn,
              is.numeric(rho))

    if (length(rho) == 1){
      rho <- rep(rho, nn)
    }
  }

  # Initialize vectors to store the 1x2 probabilities in.
  probd <- numeric(nn)
  prob1 <- numeric(nn)
  prob2 <- numeric(nn)

  for (ii in 1:nn){

    # Compute the matrix of goal probabilities, depending on the model.
    # This section is copypasta from predict_goals().
    if (model == 'poisson'){
      scoremat <- stats::dpois(0:uprx, expg1[ii]) %*% t(stats::dpois(0:uprx, expg2[ii]))
    } else if (model == 'negbin'){
      scoremat <- stats::dnbinom(0:uprx, mu = expg1[ii], size = 1 / dispersion[ii]) %*%
        t(stats::dnbinom(0:uprx, mu = expg2[ii], size = 1 / dispersion[ii]))
    } else if (model == 'cmp'){
      # Convert the expected goals to CMP-lambda.
      ll1 <- lambdaCMP(mu = expg1[ii], upsilon = dispersion[ii], method = 'fast')
      ll2 <- lambdaCMP(mu = expg2[ii], upsilon = dispersion[ii], method = 'fast')

      scoremat <- dCMP(0:uprx, lambda = ll1, upsilon = dispersion[ii]) %*%
        t(dCMP(0:uprx, lambda = ll2, upsilon = dispersion[ii]))
    }

    # Dixon-Coles adjustemt.
    if (!is.null(rho)){
      tau_matrix <- matrix(tau(goals1 = c(0,1,0,1), goals2 = c(0,0,1,1),
                               lambda1 = rep(expg1[ii], 4), lambda2 = rep(expg2[ii], 4),
                               rho = rho[ii]),
                           nrow=2)

      scoremat[1:2, 1:2] <- scoremat[1:2, 1:2] * tau_matrix
    }

    # Compute 1x2 probabilities.
    probd[ii] <- sum(diag(scoremat))
    prob1[ii] <- sum(scoremat[lower.tri(scoremat)])
    prob2[ii] <- 1 - (probd[ii] + prob1[ii])

  }

  res <- cbind(prob1, probd, prob2)
  colnames(res) <- c('p1', 'px', 'p2')

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
                      stringsAsFactors = FALSE,
                      row.names = NULL)

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
                      stringsAsFactors = FALSE,
                      row.names = NULL)
  } else {
    names(prob_under) <- NULL
    names(prob_over) <- NULL
    out <- list(prob_under=prob_under, prob_over=prob_over)
  }

  return(out)

}



#' Compute both-teams-to-score probabilities from expected goals.
#'
#' @param expg1 Non-negative numeric. The expected number of goals.
#' @param expg2 Non-negative numeric. The expected number of goals.
#' @param model String indicating whether the goals follow a 'poisson' model (default), a Negative Binomial ('negbin'), or Conway-Maxwell-Poisson ('cmp') model.
#' @param dispersion Non-negative numeric. The dispersion parameter in the Negative Binomial model or the Conway-Maxwell-Poisson model.
#' @param rho Numeric. The Dixon-Coles adjustment.
#'
#' @return
#' A numeric vector with probabilities for both teams to score.
#'
#' @export
pbtts <- function(expg1, expg2, model = 'poisson', dispersion = NULL, rho = NULL){

  stopifnot(length(expg1) == length(expg2),
            all(expg1 >= 0),
            all(expg2 >= 0),
            length(model) >= 1,
            is.numeric(expg1), is.numeric(expg2),
            model %in% c('poisson', 'negbin', 'cmp'))

  nn <- length(expg1)

  names(expg1) <- NULL
  names(expg2) <- NULL

  if (model %in% c('negbin', 'cmp') & is.null(dispersion)){
    stop('Dispersion parameter not provided.')
  }


  if (!is.null(dispersion)){
    stopifnot(length(dispersion) == 1 | length(dispersion) == nn,
              is.numeric(dispersion),
              all(dispersion > 0))

    if (length(dispersion) == 1){
      dispersion <- rep(dispersion, nn)
    }

    if (model == 'poisson'){
      warning('Dispersion parameter will be ignored for model = "poisson".')
    }

  }

  if (!is.null(rho)){
    stopifnot(length(rho) == 1 | length(rho) == nn,
              is.numeric(rho))

    if (length(rho) == 1){
      rho <- rep(rho, nn)
    }
  }


  if (model == 'poisson'){
    res <- (1 - stats::dpois(0, lambda = expg1)) * (1 - stats::dpois(0, lambda = expg2))
  } else if (model == 'negbin'){
    res <- (1 - stats::dnbinom(0, mu = expg1, size = 1 / dispersion)) *
      (1 - stats::dnbinom(0, mu = expg2, size = 1 / dispersion))
  } else if (model == 'cmp'){
    # Convert the expected goals to CMP-lambda.
    ll1 <- lambdaCMP(mu = expg1, upsilon = dispersion, method = 'fast')
    ll2 <- lambdaCMP(mu = expg2, upsilon = dispersion, method = 'fast')

    res <- (1 - dCMP(0, lambda = ll1, upsilon = dispersion)) *
      (1 - dCMP(0, lambda = ll2, upsilon = dispersion))
  }

  stopifnot(length(res) == length(expg1))

  # Dixon-Coles
  if (!is.null(rho)){

    # Compute the probability of 1-1 score, which is needed to compute
    # the DC adjustment.

    if (model == 'poisson'){
      p11 <- stats::dpois(1, lambda = expg1) * stats::dpois(1, lambda = expg2)
    } else if (model == 'negbin'){
      p11 <- stats::dnbinom(1, mu = expg1, size = 1 / dispersion) *
        stats::dnbinom(1, mu = expg2, size = 1 / dispersion)
    } else if (model == 'cmp'){
      p11 <- dCMP(1, lambda = ll1, upsilon = dispersion) *
        dCMP(1, lambda = ll2, upsilon = dispersion)
    }

    dc_adj <- (p11 * (1-rho)) - p11

    stopifnot(length(dc_adj) == length(res))

    res  <- res + dc_adj

  }

  return(res)

}


#' @rdname predict_expg
#' @export
predict_btts <- function(model_fit, team1, team2,
                           x1=NULL, x2=NULL, return_df = FALSE){


  stopifnot(length(team1) == length(team2))

  ## Compute bivariate probability distribution of goals.
  dgoals <- predict_expg(model_fit, team1 = team1, team2 = team2,
                          x1 = x1, x2 = x2)

  res <- pbtts(expg1 = dgoals$expg1, expg2 = dgoals$expg2,
              model = model_fit$model,
              dispersion = model_fit$parameters$dispersion,
              rho = model_fit$parameters$rho)


  if (return_df){
    out <- data.frame(team1 = team1, team2 = team2,
                      prob_btts = res,
                      stringsAsFactors = FALSE,
                      row.names = NULL)

  } else {
    out <- list(prob_btts = res)
  }

  return(out)

}


