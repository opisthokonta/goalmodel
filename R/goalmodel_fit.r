# This script contain functions for fitting goalmodels.


# Find initial values
initial_values <- function(goals1, goals2, team1, team2, all_teams,
                           offset = NULL, weights=NULL){
  # Compute initial values for the attack and defence effects.

  n_teams <- length(all_teams)

  parameter_list <- list(attack = rep(0.0, n_teams),
                         defense = rep(0.0, n_teams))
  names(parameter_list$attack) <- all_teams
  names(parameter_list$defense) <- all_teams

  avg <- mean(c(goals2, goals1))

  for (tt in 1:length(all_teams)){
    tt_idx1 <- team1 == all_teams[tt]
    tt_idx2 <- team2 == all_teams[tt]

    if (is.null(weights)){
      avg_scored <- mean(c(goals2[tt_idx2], goals1[tt_idx1]))
      avg_conceded <- mean(c(goals1[tt_idx2], goals2[tt_idx1]))
    } else {
      ww <- c(weights[tt_idx2], weights[tt_idx1])
      avg_scored <- stats::weighted.mean(c(goals2[tt_idx2], goals1[tt_idx1]), ww)
      avg_conceded <- stats::weighted.mean(c(goals1[tt_idx2], goals2[tt_idx1]), ww)
    }

    parameter_list$attack[all_teams[tt]] <- (avg - avg_scored)/2.5
    parameter_list$defense[all_teams[tt]] <- (avg - avg_conceded)/2.5
  }

  parameter_list$defense <- parameter_list$defense[-1]
  parameter_list$intercept <- avg

  if (!is.null(offset)){
    moffset <- exp(mean(offset))
    parameter_list$attack <- parameter_list$attack / moffset
    parameter_list$defense <- parameter_list$defense / moffset
    parameter_list$intercept <- moffset
  }

  return(parameter_list)
}


# Compute the Rue-Salvesen adjustment.
delta_ab <- function(attack1, defense1, attack2, defense2){
  # from Rue & Salvesen (1997), section 2.2.
  dd <- (attack1 + defense1 - attack2 - defense2) / 2

  return(dd)
}

# Compute the Dixon-Coles adjustment to the log-likelihood.
tau <- function(goals1, goals2, lambda1, lambda2, rho){
  taufactor <- rep(1, length(goals1))
  if (rho == 0){
    return(taufactor)
  }
  y0 <- goals2 == 0
  y1 <- goals2 == 1
  x0 <- goals1 == 0
  x1 <- goals1 == 1
  idx00 <- x0 & y0
  taufactor[idx00] <- 1 - lambda1[idx00] * lambda2[idx00] * rho
  idx01 <- x0 & y1
  taufactor[idx01] <- 1 + lambda1[idx01] * rho
  idx10 <- x1 & y0
  taufactor[idx10] <- 1 + lambda2[idx10] * rho
  idx11 <- x1 & y1
  taufactor[idx11] <- 1 - rho
  return(taufactor)
}


# Compute the expected value in goalmodel.
#
# Compute the expected value for the poisson and negative binomial
# from a list of paramteres and data. This function is used many
# places, like when making predictions and in the negloklik function.
#
lambda_pred <- function(plist, team1, team2, x1, x2){

  # Team 1 log-expected goals
  eta1 <- plist$intercept + plist$attack[team1] - plist$defense[team2]

  if ('hfa' %in% names(plist)){
    eta1 <- eta1 + plist$hfa
  }

  # Team 2 log- expected goals
  eta2 <- plist$intercept + plist$attack[team2] - plist$defense[team1]

  # Psychological underestimation factor (Rue & Salvesen 1997).
  if ('gamma' %in% names(plist)){
    deltas <- delta_ab(plist$attack[team1], plist$defense[team1],
                       plist$attack[team2], plist$defense[team2])
    gamma_delta <- plist$gamma * deltas
    eta1 <- eta1 - gamma_delta
    eta2 <- eta2 + gamma_delta
  }

  # Additional covariates.
  if (!is.null(x1)){
    column_order1 <- match(names(plist$beta), colnames(x1))
    if (length(column_order1) != 0){ # if 0, no matching covariates found.
      x1 <- x1[,stats::na.omit(column_order1), drop=FALSE]
      eta1 <- eta1 + as.numeric((x1 %*% plist$beta[!is.na(column_order1)]))
    }
  } else {
    # Length 0 to check later on and give warning.
    column_order1 <- numeric()
  }

  if (!is.null(x2)){
    # re-arrange the x2 matrix, to allow proper matrix multiplication.
    # Columns missing in the matrix, are asumed to be 0.
    column_order2 <- match(names(plist$beta), colnames(x2))
    if (length(column_order1) != 0){
      x2 <- x2[,stats::na.omit(column_order2), drop=FALSE]
      eta2 <- eta2 + as.numeric((x2 %*% plist$beta[!is.na(column_order2)]))
    }
  } else {
    column_order2 <- numeric()
  }

  if (is.null(x1) & is.null(x2) & !is.null(plist$beta)){
    warning('Covariate matrix missing. All covariates assumed to be 0.')
  }

  if ((!is.null(x1) | !is.null(x2)) & is.null(plist$beta)){
    warning('Covariate matrix provided, but the model has no matching parameters. They are ignored.')
  }

  # link function.
  lambda1 <- exp(eta1)
  lambda2 <- exp(eta2)

  out <- list(expg1 = lambda1, expg2 = lambda2)
  return(out)

}



# The negative log-likelihood function for the goalmodel
negloglik <- function(params, goals1, goals2, team1, team2,
                      x1, x2, hfa, model, param_skeleton,
                      weights=NULL, fixed_params=NULL){

  # relist, to make things easier.
  plist <- utils::relist(params, param_skeleton)


  # Add sum to zero constraint on defense parameters.
  # The defense paramter for the first team is computed from the rest.
  plist$defense <- c(sum(plist$defense)*-1, plist$defense)
  names(plist$defense)[1] <- names(plist$attack[1]) # add name to first element.

  if (!is.null(fixed_params)){
    stopifnot(is.list(fixed_params))
    # Add the fixed parameters to the parameter list.
    plist <- utils::modifyList(plist, fixed_params)
  }


  ## Expected goals (poisson & nbin parameters)
  expg <- lambda_pred(plist, team1, team2, x1, x2)

  # The log-likelihood
  if (model == 'poisson'){
    log_lik_1 <- stats::dpois(goals1, lambda = expg$expg1, log=TRUE)
    log_lik_2 <- stats::dpois(goals2, lambda = expg$expg2, log=TRUE)
  } else if (model == 'negbin'){
    dispersion_tmp <- 1 / exp(plist$dispersion)
    log_lik_1 <- stats::dnbinom(goals1, mu = expg$expg1, size = dispersion_tmp, log=TRUE)
    log_lik_2 <- stats::dnbinom(goals2, mu = expg$expg2, size = dispersion_tmp, log=TRUE)
  }

  log_lik_terms <- log_lik_1 + log_lik_2


  # Dixon-Coles adjustment
  if ('rho' %in% names(plist)){

    dc_adj <- tau(goals1, goals2, expg$expg1, expg$expg2, rho = plist$rho)

    # Trick to avoid warnings.
    if (any(dc_adj <= 0.0)){
      return(Inf)
    }

    log_lik_terms <- log_lik_terms + log(dc_adj)
  }

  # sum the log likelihood.
  if (!is.null(weights)){
    log_lik <- sum(log_lik_terms*weights)
  } else {
    log_lik <- sum(log_lik_terms)
  }

  return(log_lik*-1)

}

#' Fitting models for goals
#'
#' \code{goalmodel} is used to fit models of goals scored in sports competitions. At a minimum this function estimates 'attack' and 'defence'
#' ratings for all teams, but other covariates can be included, as well as other adjustments. The underlying statistical model
#' can be either a Poisson or a Negative Binomial model.
#'
#' @param goals1 Numeric, non-negative integer. The number of goals scored by team 1.
#' @param goals2 Numeric, non-negative integer. The number of goals scored by team 2.
#' @param team1 Vector of team names.
#' @param team2 Vector of team names.
#' @param x1 Matrix of additional covariates associated with team 1.
#' @param x2 Matrix of additional covariates associated with team 2.
#' @param hfa Logical (TRUE by default). Include a (home field) advantage for team 1.
#' @param dc Logical (FALSE by default). If TRUE an adjustment for low scoring goals is included in the model.
#' @param rs Logical (FALSE by default). If TRUE an adjustment for teams to over and under estimate the opponent.
#' @param fixed_params A list with parameters that should be kept constant while the other parameters are estimated from data.
#' @param weights Numeric vector of weigths that determine the influence of each match on the final parameter estimates.
#' @param model String indicating whether the goals follow a 'poisson' model (default) or a 'negbin' model.
#' @param optim_method String indicating which optimization method to use. See \code{\link{optim}} for more details.
#'
#' Fixed parameters must be given as a list that is similar as the one returned from this function. See the 'value' section.
#'
#' @return
#'  A list of class 'goalmodel'. The list contains the following components:
#'  \itemize{
#' \item parameters - A list of parameters.
#' \item loglikelihood - The value of the log-likelihood at the maximum.
#' \item npar_est - Number of parameters that has been estimated.
#' \item npar_fixed - Number of parameter that was fixed to a constant value during model fitting.
#' \item aic - Akaike's Information Criterium.
#' \item r_squared - A pseudo r-squared; The proportion of variability in the observed goals that the model explains.
#' \item all_teams - A character vector with the names of all the teams in the data.
#' \item ngames -  Number of games in the data.
#' \item est_time - The time it took to fit the model.
#' \item model = A string indicating whether a Poisson or Negative Binomial model was used.
#' \item optim_res - The output from optim.
#' \item fixed_params - A list with the parameters that was kept constant during model fitting.
#' \item maxgoal - The greatest number of goals seen in the data. Used by the prediction functions.
#'  }

#'
#' The parameters list contain both the fixed and estimated parameters. It contains the following compnents:
#' \itemize{
#' \item attack - A named mumeric of the attack parameters.
#' \item defense - A named mumeric of the attack parameters.
#' \item intercept - an unnamd length 1 numeric with the intercept.
#' }
#' The following parameter components are optional, and depends on what kind of model is fitted.
#' \itemize{
#' \item hfa - If hfa=TRUE, an unnamd length 1 numeric with the home field advantage.
#' \item rho - If dc=TRUE, an unnamd length 1 numeric with the Dixon-Coles adjustment.
#' \item gamma - If rs=TRUE, an unnamd length 1 numeric with the Rue-Salvesen adjustment.
#' \item beta - If additional covarites are used, this is a named mumeric of the regression coefficients.
#' }
#'
#' @examples
#'
#' # See the vignette for examples.
#'
#' @export
goalmodel <- function(goals1, goals2, team1, team2,
                      x1 = NULL, x2=NULL,
                      hfa=TRUE, dc=FALSE, rs=FALSE,
                      fixed_params = NULL, weights=NULL,
                      model = 'poisson', optim_method='BFGS'){

  stopifnot(length(goals1)==length(goals2),
            length(goals2) == length(team1),
            length(team1) == length(team2),
            length(goals1) >= 1,
            is.numeric(goals1), is.numeric(goals2),
            model %in% c('poisson', 'negbin')) # only poisson model for now.

  if (!is.null(weights)){
    stopifnot(is.numeric(weights),
              length(goals1)==length(weights),
              all(weights >= 0),
              all(!is.na(weights)),
              !all(weights == 0))
  }

  team1 <- as.character(team1)
  team2 <- as.character(team2)
  all_teams <- sort(unique(c(unique(team1), unique(team2))), decreasing = FALSE)
  n_teams <- length(all_teams)

  parameter_list <- initial_values(goals1 = goals1, goals2 = goals2,
                                   team1 = team1, team2=team2,
                                   all_teams = all_teams)

  if (hfa){
    parameter_list$hfa <- 0.1
  }

  if (dc){
    parameter_list$rho <- 0.01
  }

  if (rs){
    parameter_list$gamma <- 0.0
  }

  if (model == 'negbin'){
    # on log scale during estimation.
    parameter_list$dispersion <- -10
  }

  # Parameters for additional covariates.
  additional_covariates <- c() # the variable names.
  if (!is.null(x1)){
    stopifnot(is.matrix(x1))

    if (is.null(colnames(x1))){
      stop('Error: The columns of the x1 matrix must be named.')
    }
    additional_covariates <- unique(c(additional_covariates, colnames(x1)))
  }

  if (!is.null(x2)){
    stopifnot(is.matrix(x2))

    if (is.null(colnames(x2))){
      stop('Error: The columns of the x2 matrix must be named.')
    }

    additional_covariates <- unique(c(additional_covariates, colnames(x2)))
  }

  if (length(additional_covariates) != 0){
    parameter_list$beta <- rep(0.1, length(additional_covariates))
    names(parameter_list$beta) <- additional_covariates
  }

  if (!is.null(fixed_params)){
    stopifnot(is.list(fixed_params))
    # remove fixed parameters from the parameter_list, since they are
    # not optimized over.
    parameter_list[names(fixed_params)] <- NULL

    if ('dispersion' %in% names(fixed_params)){
      # During estimation, the dispersion parameter is on the log scale
      # to avoid negative values.
      fixed_params$dispersion <- log(fixed_params$dispersion)

      if (model == 'poisson'){
        warning('Dispersion parameter is fixed, but model is Poisson. The dispersion parameter will not have an effect.')
      }
    }

  }

  # Commence estimation.
  parameter_vector <- unlist(parameter_list)
  start_time <- Sys.time()
  optim_res <- stats::optim(par = parameter_vector, fn=negloglik,
                     goals1 = goals1, goals2 = goals2,
                     team1=team1, team2=team2,
                     x1 = x1, x2 = x2,
                     fixed_params=fixed_params,
                     model = model,
                     param_skeleton=parameter_list,
                     weights = weights,
                     method = optim_method)

  end_time <- Sys.time()
  est_time <- difftime(end_time, start_time, units='secs')

  if (optim_res$convergence != 0){
    warning('Did not converge!! Parameter estimates are unreliable.')
  }

  # relist the parameter vector, calculate the missing defense parameter.
  parameter_list_est <- utils::relist(optim_res$par, parameter_list)
  parameter_list_est$defense <- c(sum(parameter_list_est$defense)*-1, parameter_list_est$defense)
  names(parameter_list_est$defense)[1] <- names(parameter_list_est$attack[1])

  loglikelihood <- optim_res$value*-1
  npar_est <- length(optim_res$par)
  npar_fixed <- length(unlist(fixed_params))
  aic <- npar_est*2 - 2*loglikelihood

  ## Deviances
  if (model == 'poisson'){
    all_goals <- c(goals1, goals2)
    if (is.null(weights)){
      loglikelihood_saturated <- sum(stats::dpois(all_goals, lambda = all_goals, log=TRUE))
      loglikelihood_null <- sum(stats::dpois(all_goals, lambda = mean(all_goals), log=TRUE))
    } else {
      loglikelihood_saturated <- sum(stats::dpois(all_goals, lambda = all_goals, log=TRUE)*rep(weights,2))
      loglikelihood_null <- sum(stats::dpois(all_goals, lambda = stats::weighted.mean(all_goals, w = rep(weights, 2)), log=TRUE)*weights)
    }

  } else if (model == 'negbin'){
    all_goals <- c(goals1, goals2)
    if (is.null(weights)){
      mean_goals <- mean(all_goals)
      dispersion0_tmp <- MASS::theta.ml(y = all_goals, mu=mean_goals, limit = 1000)
      loglikelihood_saturated <- sum(stats::dnbinom(all_goals, mu = all_goals, size=Inf, log=TRUE))
      loglikelihood_null <- sum(stats::dnbinom(all_goals, mu = mean_goals, size=dispersion0_tmp, log=TRUE))
    } else {
      mean_goals <- stats::weighted.mean(all_goals, w = rep(weights, 2))
      dispersion0_tmp <- MASS::theta.ml(y = all_goals, mu=mean_goals, weights = rep(weights,2), limit = 1000)
      loglikelihood_saturated <- sum(stats::dnbinom(all_goals, mu = all_goals, size=Inf, log=TRUE)*rep(weights,2))
      loglikelihood_null <- sum(stats::dnbinom(all_goals, mu = mean_goals, size=dispersion0_tmp, log=TRUE)*rep(weights,2))
    }
  }

  deviance <- 2 * (loglikelihood_saturated - loglikelihood)
  deviance_null <- 2 * (loglikelihood_saturated - loglikelihood_null)
  r_squared <- 1 - (deviance / deviance_null)

  ngames <- length(goals1)

  # Add the fixed parameters to the parameter list.
  if (!is.null(fixed_params)){
    stopifnot(is.list(fixed_params))
    parameter_list_all <- utils::modifyList(parameter_list_est, fixed_params)
  } else {
    parameter_list_all <- parameter_list_est
  }

  # rescale dispersion
  if ('dispersion' %in% names(parameter_list_all)){
    parameter_list_all$dispersion <- exp(parameter_list_all$dispersion)
  }

  # maxgoal. Useful for later predictions.
  maxgoal <- max(max(goals1), max(goals2))

  out <- list(parameters = parameter_list_all,
              loglikelihood=loglikelihood, npar_est=npar_est,
              npar_fixed = npar_fixed, aic=aic, r_squared=r_squared,
              all_teams = all_teams, ngames = ngames,
              est_time=est_time, model = model, optim_res=optim_res,
              fixed_params=fixed_params,
              maxgoal = maxgoal)

  class(out) <- 'goalmodel'

  return(out)

}





