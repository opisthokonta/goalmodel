

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
# from a list of parameters and data. This function is used many
# places, like when making predictions and in the negloglik function.
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

# Function to check that the network of all team that has played eachother
# is fully connected.
is_connected <- function(edgelist){

  all_nodes <- sort(unique(c(edgelist[,1], edgelist[,2]))) # all nodes
  n_nodes <- length(all_nodes) # number of nodes

  start_node <- all_nodes[1]

  # initiate the lgocial vector for nodes that has been reachef from the
  # starting node.
  reachable_from_start <- all_nodes == start_node

  # logical of length n_nodes, indicating nodes that has been visited.
  # These should not be added to queue, hence the name dequed.
  dequed_nodes <- all_nodes == start_node

  # logical of length n_teams, indicating queue.
  node_queue <- all_nodes == start_node

  while (sum(node_queue) != 0){

    # First node in the queue.
    cur_node <- all_nodes[node_queue][1]

    reachable_from_current <- union(edgelist[edgelist[,1] == cur_node,2], edgelist[edgelist[,2] == cur_node,1])
    reachable_from_current_idx <- all_nodes %in% reachable_from_current & (!dequed_nodes)
    reachable_from_start[reachable_from_current_idx] <- reachable_from_start[reachable_from_current_idx] | TRUE

    # list of nodes that has been visited, and had their neighbors visited.
    dequed_nodes[all_nodes == cur_node] <- TRUE

    # Update node queue
    node_queue <- (node_queue | (all_nodes %in% reachable_from_current)) & (!dequed_nodes)

    if (sum(reachable_from_start) == n_nodes){break}

  }

  sum(reachable_from_start) == n_nodes

}




# Check that a list of parameters has correct layout.
# Returns TRUE if everything is OK.
check_plist <- function(plist, all_teams = NULL){

  t1 <- is.list(plist)

  plist_names <- names(plist)

  t2 <- all(plist_names %in% c('attack', 'defense', 'beta', 'intercept',
                               'sigma', 'rho', 'dispersion', 'gamma', 'hfa'))

  t3 <- all(sapply(plist, is.numeric))

  # check the length of the vectors in the list.
  t4 <- TRUE
  for (ii in 1:length(plist_names)){
    if (plist_names[ii] %in% c('intercept', 'sigma', 'dispersion', 'rho', 'gamma', 'hfa')){
      t4 <- t4 & length(plist[[plist_names[ii]]] == 1)
    }
  }

  # (Optional) Check that the attack and defence parameters
  # are available for all teams.
  t5 <- TRUE
  if (!is.null(all_teams)){
    if ('attack' %in% plist_names){
      t5 <- t5 & all(names(plist$attack) %in% all_teams)
    }

    if ('defense' %in% plist_names){
      t5 <- t5 & all(names(plist$defense) %in% all_teams)
    }

  }


  out <- all(t1, t2, t3, t4)
  return(out)
}


# Internal function to augment the parameter list with the fixed parameters.
fill_fixed_params <- function(plist, fixed_params = NULL){

  if (!is.null(fixed_params)){

    stopifnot(is.list(fixed_params))

    if ('attack' %in% names(fixed_params)){
      plist$attack <- c(plist$attack, fixed_params$attack)
      fixed_params$attack <- NULL # trick to make the modifyList function work later.
    }

    if ('defense' %in% names(fixed_params)){
      plist$defense <- c(plist$defense, fixed_params$defense)
      fixed_params$defense <- NULL
    }

    # If some fixed parameters are given only to one of attack or defense,
    # and the other is not in plist, add it as missing.
    missing_from_attack_idx <- which(!names(plist$defense) %in% names(plist$attack))
    if (length(missing_from_attack_idx) > 0){
      plist$attack[names(plist$defense)[missing_from_attack_idx]] <- NA
    }

    missing_from_defense_idx <- which(!names(plist$attack) %in% names(plist$defense))
    if (length(missing_from_defense_idx) > 0){
      plist$defense[names(plist$attack)[missing_from_defense_idx]] <- NA
    }



    # Add the any other fixed parameters to the parameter list.
    if (any(!names(fixed_params) %in% c('attack', 'defense'))){
      plist <- utils::modifyList(plist, fixed_params)
    }

  }

  return(plist)

}

# The negative log-likelihood function for the goalmodel
negloglik <- function(params, goals1, goals2, team1, team2,
                      x1, x2, hfa, model, param_skeleton,
                      all_teams,
                      weights=NULL, fixed_params=NULL){

  # relist, to make things easier.
  plist <- utils::relist(params, param_skeleton)

  # TODO: There is a bug here, i think, if one of the teams with
  # a fixed attack or defense parameter is the first team
  # in the all_teams vector.

  # Add sum to zero constraint on defense parameters.
  # The defense parameter for the first team is computed from the rest.
  if (length(plist$defense) != 0){
    # if 0, then all defense parameters are presumably fixed.
    plist$defense <- c(sum(plist$defense)*-1, plist$defense)
    names(plist$defense)[1] <- all_teams[1] # add name to first element.
  }

  if (length(plist$attack) != 0){
    # if 0, then all attack parameters are presumably fixed.
    plist$attack <- c(sum(plist$attack)*-1, plist$attack)
    names(plist$attack)[1] <- all_teams[1] # add name to first element.
  }

  # Add the fixed parameters to the parameter list.
  plist <- fill_fixed_params(plist, fixed_params = fixed_params)

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
  } else if (model == 'gaussian'){
    log_lik_1 <- stats::dnorm(goals1, mean = expg$expg1, sd = exp(plist$sigma), log=TRUE)
    log_lik_2 <- stats::dnorm(goals2, mean = expg$expg2, sd = exp(plist$sigma), log=TRUE)
  } else if (model == 'cmp'){
    exp_log_upsilon <- exp(plist$dispersion)

    # This is a dirty hack essentially setting hard upper and lower
    # bounds for the the dispersion parameter.
    if (exp_log_upsilon < 0.7){return(Inf)}
    if (exp_log_upsilon > 1.7){return(Inf)}

    log_lik_1 <- dCMP(goals1, lambda = lambdaCMP(mu=expg$expg1,
                                      upsilon = exp_log_upsilon,
                                      method = 'fast'),
                             upsilon = exp_log_upsilon, log=TRUE)
    log_lik_2 <- dCMP(goals2, lambda = lambdaCMP(mu=expg$expg2,
                                                upsilon = exp_log_upsilon,
                                                method = 'fast'),
                             upsilon = exp_log_upsilon, log=TRUE)
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


  if ('hurdle' %in% names(plist)){

    zz_idx <- goals1 == 0 & goals2 == 0

    hurdle_adj <- exp(plist$hurdle) * exp(-(expg$expg1 + expg$expg2))

    if (any(hurdle_adj >= 1)){
      return(Inf)
    }

    log_lik_terms[zz_idx] <- log(hurdle_adj[zz_idx])
    log_lik_terms[!zz_idx] <- log(1 - hurdle_adj[!zz_idx]) + log_lik_terms[!zz_idx]

  }

  # sum the log likelihood.
  if (!is.null(weights)){
    log_lik <- sum(log_lik_terms*weights)
  } else {
    log_lik <- sum(log_lik_terms)
  }

  return(log_lik*-1)

}


# Fit models using the built-in glm.fit() function.
# This is an internal function that in some cases provides the final parameter
# estimates. When model = 'negbin' or 'cmp', or when there are fixed
# coefficients, this function provides only starting values for later
# optimization. Therefore the output of this function (coefficeints, loglik)
# does not neccecarily reflect the input.
gm_fit_glm <- function(goals1, goals2, team1, team2,
                           all_teams, model, additional_covariates,
                           x1 = NULL, x2=NULL,
                           hfa=TRUE,  weights=NULL){

  # prepare some
  n_teams <- length(all_teams)
  team1_stacked <- c(team1, team2)
  team2_stacked <- c(team2, team1) # Opponent

  # Make response (y) vector
  yy <- c(goals1, goals2)

  # Attack dummy matrix
  xmata <- matrix(0, nrow=length(goals2)*2,
                  ncol = (n_teams-1))
  colnames(xmata) <- all_teams[-1]

  # Defense dummy matrix
  xmatd <- matrix(0, nrow=length(goals2)*2,
                  ncol = (n_teams-1))
  colnames(xmatd) <- all_teams[-1]

  for (ii in 2:n_teams){

    t1_idx <- team1_stacked == all_teams[ii]
    t2_idx <- team2_stacked == all_teams[ii]
    xmata[t1_idx, all_teams[ii]] <- 1
    xmatd[t2_idx, all_teams[ii]] <- -1
  }

  # Sum-to-zero constraint for the first team.
  xmata[team1_stacked == all_teams[1]] <- -1
  xmatd[team2_stacked == all_teams[1]] <- -1

  # Combine the attack and defence matrices.
  colnames(xmata) <- paste(colnames(xmata), 'Attack', sep='_')
  colnames(xmatd) <- paste(colnames(xmatd), 'Defense', sep='_')

  # Add home field advantage.
  if (hfa){
    xmat <- cbind(intercept = 1, hfa = rep(1:0, each=length(goals2)),
                  xmata, xmatd)
  } else {
    xmat <- cbind(intercept = 1, xmata, xmatd)
  }

  # Add additional covariates.
  if (length(additional_covariates) != 0){
    additional_xmat <- matrix(0, ncol = length(additional_covariates),
                              nrow=length(goals2)*2)
    colnames(additional_xmat) <- additional_covariates

    if (!is.null(x1)){
      additional_xmat[1:length(goals2),colnames(x1)] <- x1
    }

    if (!is.null(x2)){
      additional_xmat[(length(goals1)+1):(length(goals2)*2), colnames(x2)] <- x2
    }
    xmat <- cbind(xmat, additional_xmat)
  }

  if (model %in% c('poisson', 'negbin', 'cmp')){
    # Note that a poisson model is fitted if model = 'negbin' or 'cmp'.
    glm_family <- stats::poisson(link='log')
  } else if (model == 'gaussian'){
    glm_family <- stats::gaussian(link='log')
  }

  if (is.null(weights)){
    glm_res <- stats::glm.fit(x=xmat, y=yy,
                       start=c(mean(log(yy+0.5)-0.2), rep(0, ncol(xmat)-1)),
                       family=glm_family,
                       control = list(maxit=100, epsilon = 1e-9),
                       intercept = FALSE)
  } else {
    glm_res <- stats::glm.fit(x=xmat, y=yy,
                              weights=rep(weights, 2),
                              start=c(mean(log(yy+0.5)-0.2), rep(0, ncol(xmat)-1)),
                              family=glm_family,
                              control = list(maxit=100, epsilon = 1e-9),
                              intercept = FALSE)
  }

  attack_params <- glm_res$coefficients[grepl('_Attack$', names(glm_res$coefficients))]
  names(attack_params) <- sub('_Attack$', '', names(attack_params))
  attack_params <- c(sum(attack_params)*-1, attack_params)
  names(attack_params)[1] <- all_teams[1]

  defense_params <- glm_res$coefficients[grepl('_Defense$', names(glm_res$coefficients))]
  names(defense_params) <- sub('_Defense$', '', names(defense_params))
  defense_params <- c(sum(defense_params), defense_params)
  names(defense_params)[1] <- all_teams[1]

  param_list <- list(attack = attack_params,
                     defense = defense_params,
                     intercept = glm_res$coefficients['intercept'])

  if (model == 'gaussian'){
    param_list$sigma = stats::sd(glm_res$residuals)
  }

  names(param_list$intercept) <- NULL # The intercept vector should not be named.

  if (hfa){
    param_list$hfa <- glm_res$coefficients['hfa']
    names(param_list$hfa) <- NULL # The hfa vector should not be named.
  }

  if (length(additional_covariates) != 0){
    nadc <- length(additional_covariates)
    ncoef <- length(glm_res$coefficients)
    param_list$beta <- glm_res$coefficients[(ncoef-(nadc-1)):ncoef]
  }

  stopifnot(check_plist(param_list))

  # Compute the log-likelihood.
  if (model %in% c('poisson', 'negbin', 'cmp')){
    if (is.null(weights)){
      loglik <- sum(stats::dpois(x = c(goals1, goals2),
                                 lambda=glm_res$fitted.values, log=TRUE))
    } else {
      loglik <- sum(stats::dpois(x = c(goals1, goals2),
                                 lambda=glm_res$fitted.values, log=TRUE)*rep(weights, 2))
    }

  } else if (model == 'gaussian'){
    if (is.null(weights)){
    loglik <- sum(stats::dnorm(x = c(goals1, goals2),
                               mean=glm_res$fitted.values, log=TRUE))
    } else {
      loglik <- sum(stats::dnorm(x = c(goals1, goals2),
                                 mean=glm_res$fitted.values, log=TRUE)*rep(weights,2))
    }
  }


  return(list(parameters = param_list, loglikelihood=loglik,
              npar_fixed = 0, npar_est = ncol(xmat), aic=glm_res$aic,
              converged = glm_res$converged,
              boundary = glm_res$boundary))

}




#' Fitting models for goals
#'
#' \code{goalmodel} is used to fit models of goals scored in sports competitions. At a minimum this function estimates 'attack' and 'defence'
#' ratings for all teams, but other covariates can be included, as well as other adjustments. The underlying statistical model
#' can be either a Poisson, Negative Binomial, or Gaussian model.
#'
#' Fixed parameters must be given as a list that is similar as the one returned from this function. See the 'value' section.
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
#' @param hurdle Logical (FALSE by default). If TRUE, 0-0 results will be modeled using a separate parameter (a hurdle model).
#' @param fixed_params A list with parameters that should be kept constant while the other parameters are estimated from data.
#' @param weights Numeric vector of weigths that determine the influence of each match on the final parameter estimates.
#' @param model String indicating whether the goals follow a 'poisson' model (default), a Negative Binomial ('negbin'), Conway-Maxwell-Poisson ('cmp') or a Gaussian ('gaussian') model.
#' @param optim_method String indicating which optimization method to use in case the model can't be fitted with gml.fit(). See \code{\link{optim}} for more details.
#'
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
#' \item fixed_params - A list with the parameters that was kept constant during model fitting.
#' \item converged - Logical indicating if the model fitting converged.
#' \item maxgoal - The greatest number of goals seen in the data. Used by the prediction functions.
#' \item fitter - String indicating wether the model was fitted with the built-in glm.fit() function.
#'  }
#'
#' The parameters list contain both the fixed and estimated parameters. It contains the following compnents:
#' \itemize{
#' \item attack - A named mumeric of the attack parameters.
#' \item defense - A named mumeric of the attack parameters.
#' \item intercept - A length 1 numeric with the intercept.
#' }
#' The following parameter components are optional, and depends on what kind of model is fitted.
#' \itemize{
#' \item hfa - If hfa=TRUE, an unnamd length 1 numeric with the home field advantage.
#' \item rho - If dc=TRUE, an unnamd length 1 numeric with the Dixon-Coles adjustment.
#' \item gamma - If rs=TRUE, an unnamd length 1 numeric with the Rue-Salvesen adjustment.
#' \item beta - If additional covarites are used, this is a named mumeric of the regression coefficients.
#' \item dispersion - The dispersion parameter in the Negative Binomial model or the Conway-Maxwell-Poisson model.
#' \item sigma - The standard deviation in a Gaussian model.
#' \item hurdle - The hurdle parameter.
#' }
#'
#' @examples
#'
#' # See the vignette for examples.
#'
#' @export
goalmodel <- function(goals1, goals2, team1, team2,
                      x1 = NULL, x2=NULL,
                      hfa=TRUE, dc=FALSE, rs=FALSE, hurdle = FALSE,
                      fixed_params = NULL, weights=NULL,
                      model = 'poisson', optim_method='BFGS'){

  warning_messages <- c()

  stopifnot(length(goals1) == length(goals2),
            length(goals2) == length(team1),
            length(team1) == length(team2),
            length(goals1) >= 1,
            is.numeric(goals1), is.numeric(goals2),
            model %in% c('poisson', 'negbin', 'gaussian', 'cmp'))

  if (dc){
    # Check if there is any data suitable for DC adjustment.
    if(!any(goals1 <= 1 & goals2 <= 1)){
      stop('Dixon-Coles adjustment is not applicable when there are no instances both teams scoring 1 goal or less.')
    }

    if (model %in% c('gaussian')){
      stop('Dixon-Coles adjustment does not work with a Gaussian model.')
    }
  }

  if (hurdle){
    if (dc){
      stop('dc and hurdle can not both be true.')
    }

    if (model != 'poisson'){
      stop("Hurdle model only works for model = 'poisson'.")
    }

    if(!any(goals1 == 0 & goals2 == 0)){
      stop('Hurdle model is not applicable when there are no instances of 0-0.')
    }

  }

  if (!is_connected(cbind(team1, team2))){
    wm_tmp <- 'The data contains two or more separate clusters of teams that are not comparable. The results may be unreliable.'
    warning_messages <- append(warning_messages, wm_tmp)
    warning(wm_tmp)
  }


  if (!is.null(weights)){
    stopifnot(is.numeric(weights),
              length(goals1)==length(weights),
              all(weights >= 0),
              all(!is.na(weights)),
              !all(weights == 0))
  }

  # If there are any additional coavriates (x1, x2):
  # Do some checks and make some preparations.
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


  # Make sure the team vectors are of the charcter type.
  team1 <- as.character(team1)
  team2 <- as.character(team2)

  # Some useful quantities.
  all_teams <- sort(unique(c(unique(team1), unique(team2))), decreasing = FALSE)
  n_teams <- length(all_teams)
  ngames <- length(goals1)

  # If it is sufficient to fit the model with glm.fit().
  mdefault <- model %in% c('poisson', 'gaussian') & dc == FALSE & rs == FALSE & hurdle == FALSE & is.null(fixed_params)

  fitter <- ifelse(mdefault, 'glm.fit', 'gm')

  # Fit a model with glm.fit. Some model classes are compatible with this function.
  # If not, the results from fitting this simples model is used as starting values.

  # TODO: If all attack and defence and hfa and intercept are fixed, this is not
  # needed for fitting the rest of the models. This step should be skiped
  # to save computing time.

  # TODO: Maybe even negbin models can be fitted with this approach, with
  # the glm.nb() function from the MASS pacakge.

  start_time <- Sys.time()

  gm_fit_glm_res <- gm_fit_glm(goals1=goals1, goals2=goals2,
                               team1=team1, team2=team2,
                               all_teams = all_teams,
                               model = model,
                               weights = weights,
                               additional_covariates = additional_covariates,
                               x1 = x1, x2=x2, hfa=hfa)


  if (mdefault){

    if (!gm_fit_glm_res$converged){
      wm_tmp <- 'Did not converge (glm.fit). Parameter estimates are unreliable.'
      warning_messages <- append(warning_messages, wm_tmp)
      warning(wm_tmp)
    }

    if (gm_fit_glm_res$boundary){
      wm_tmp <- 'glm.fit(): Fitted values on the boundary of the attainable values. Parameter estimates are unreliable.'
      warning_messages <- append(warning_messages, wm_tmp)
      warning(wm_tmp)
    }

    parameter_list <- gm_fit_glm_res$parameters
    loglikelihood <- gm_fit_glm_res$loglikelihood
    npar_est <- gm_fit_glm_res$npar_est
    npar_fixed <- gm_fit_glm_res$npar_fixed
    aic <- gm_fit_glm_res$aic
    converged <- gm_fit_glm_res$converged

  } else {

    # initial values from gm_fit_glm_res.
    parameter_list_init <- gm_fit_glm_res$parameters
    parameter_list_init$attack <- parameter_list_init$attack[-1]
    parameter_list_init$defense <- parameter_list_init$defense[-1]

    # Add additional initial values.
    if (dc){
      parameter_list_init$rho <- 0.01
    }

    if (rs){
      parameter_list_init$gamma <- 0.0
    }

    if (hurdle){
      parameter_list_init$hurdle <- 0.0
    }

    if (model == 'negbin'){
      # on log scale during estimation.
      # start with a value close to 0, which is almost Poisson.
      parameter_list_init$dispersion <- -10
    }

    if (model == 'gaussian'){
      # on log scale during estimation.
      parameter_list_init$sigma <- log(parameter_list_init$sigma)
    }

    if (model == 'cmp'){
      # on log scale during estimation.
      # start with a value exp(0)=1, which is the same as Poisson.
      parameter_list_init$dispersion <- 0
    }

    # Deal with fixed parameters.
    if (!is.null(fixed_params)){

      stopifnot(is.list(fixed_params))

      if (any(!names(fixed_params) %in% c('attack', 'defense', 'beta', 'intercept',
                                          'sigma', 'rho', 'dispersion', 'gamma', 'hfa'))){
        stop('In fixed_params: Invalid parameter name.')
      }

      # remove fixed parameters from the parameter_list_init, since they are
      # not optimized over.
      if ('attack' %in% names(fixed_params)){
        fixed_attack_params <- names(parameter_list_init$attack) %in% names(fixed_params$attack)
        parameter_list_init$attack <- parameter_list_init$attack[!fixed_attack_params]
      }

      if ('defense' %in% names(fixed_params)){
        fixed_defence_params <- names(parameter_list_init$defense) %in% names(fixed_params$defense)
        parameter_list_init$defense <- parameter_list_init$defense[!fixed_defence_params]
      }

      # remove the parameters that are not attack aand defence parameters.
      if (any(!names(fixed_params) %in% c('attack', 'defense'))){

        parameter_list_init[names(fixed_params)] <- NULL

        if ('dispersion' %in% names(fixed_params)){
          # During estimation, the dispersion parameter is on the log scale
          # to avoid negative values.
          fixed_params$dispersion <- log(fixed_params$dispersion)

          if (model == 'poisson'){
            wm_tmp <- 'Dispersion parameter is fixed, but model is Poisson. The dispersion parameter will not have an effect.'
            warning_messages <- append(warning_messages, wm_tmp)
            warning(wm_tmp)
          } else if (model == 'gaussian'){
            wm_tmp <- 'Dispersion parameter is fixed, but model is Gaussian The dispersion parameter will not have an effect. The related parameter for the Gaussian model is sigma.'
            warning_messages <- append(warning_messages, wm_tmp)
            warning(wm_tmp)
          }
        }
      }

    } # end fixed params

    # Commence estimation.
    parameter_vector <- unlist(parameter_list_init)

    optim_res <- stats::optim(par = parameter_vector, fn=negloglik,
                              goals1 = goals1, goals2 = goals2,
                              team1=team1, team2=team2,
                              x1 = x1, x2 = x2,
                              fixed_params=fixed_params,
                              model = model,
                              all_teams = all_teams,
                              param_skeleton=parameter_list_init,
                              weights = weights,
                              method = optim_method,
                              control = list(maxit = 250))

    converged <- optim_res$convergence == 0

    if (!converged){
      wm_tmp <- 'Did not converge (optim). Parameter estimates are unreliable.'
      warning_messages <- append(warning_messages, wm_tmp)
      warning(wm_tmp)
    }

    # relist the parameter vector, calculate the missing attack and defense parameter.
    parameter_list_est <- utils::relist(optim_res$par, parameter_list_init)

    if (length(parameter_list_est$defense) != 0){
      # TODO: There is probably a bug here, if there is only one parameter that
      # is not fixed. Also in the negloglik() function.

      # if 0, then all defense parameters are presumably fixed.
      parameter_list_est$defense <- c(sum(parameter_list_est$defense)*-1, parameter_list_est$defense)
      names(parameter_list_est$defense)[1] <- all_teams[1]
    }

    if (length(parameter_list_est$attack) != 0){
      # if 0, then all attack parameters are presumably fixed.
      parameter_list_est$attack <- c(sum(parameter_list_est$attack)*-1, parameter_list_est$attack)
      names(parameter_list_est$attack)[1] <- all_teams[1]
    }

    loglikelihood <- optim_res$value*-1
    npar_est <- length(optim_res$par)
    npar_fixed <- length(unlist(fixed_params))
    aic <- npar_est*2 - 2*loglikelihood

    # rescale dispersion
    if ('dispersion' %in% names(parameter_list_est)){
      parameter_list_est$dispersion <- exp(parameter_list_est$dispersion)
    }

    # rescale sigma
    if ('sigma' %in% names(parameter_list_est)){
      parameter_list_est$sigma <- exp(parameter_list_est$sigma)
    }

    # rescale hurlde parameter
    if ('hurdle' %in% names(parameter_list_est)){
      parameter_list_est$hudrle <- exp(parameter_list_est$hurdle)
    }

    # Add the fixed parameters to the parameter list.
    parameter_list <- fill_fixed_params(parameter_list_est, fixed_params = fixed_params)

  }

  end_time <- Sys.time()
  est_time <- difftime(end_time, start_time, units='secs')

  # Compute R squared.
  all_goals <- c(goals1, goals2)
  if (is.null(weights)){
    mean_goals <- mean(all_goals)
  } else {
    mean_goals <- stats::weighted.mean(all_goals, w = rep(weights, 2))
  }


  ## Deviances needed for R squared.
  if (model == 'poisson'){
    if (is.null(weights)){
      loglikelihood_saturated <- sum(stats::dpois(all_goals, lambda = all_goals, log=TRUE))
      loglikelihood_null <- sum(stats::dpois(all_goals, lambda = mean_goals, log=TRUE))
    } else {
      loglikelihood_saturated <- sum(stats::dpois(all_goals, lambda = all_goals, log=TRUE)*rep(weights,2))
      loglikelihood_null <- sum(stats::dpois(all_goals, lambda = mean_goals, log=TRUE)*rep(weights,2))
    }

  } else if (model == 'negbin'){
    if (is.null(weights)){
      dispersion0_tmp <- MASS::theta.ml(y = all_goals, mu=mean_goals, limit = 1000)
      loglikelihood_saturated <- sum(stats::dnbinom(all_goals, mu = all_goals, size=Inf, log=TRUE))
      loglikelihood_null <- sum(stats::dnbinom(all_goals, mu = mean_goals, size=dispersion0_tmp, log=TRUE))
    } else {
      dispersion0_tmp <- MASS::theta.ml(y = all_goals, mu=mean_goals, weights = rep(weights,2), limit = 1000)
      loglikelihood_saturated <- sum(stats::dnbinom(all_goals, mu = all_goals, size=Inf, log=TRUE)*rep(weights,2))
      loglikelihood_null <- sum(stats::dnbinom(all_goals, mu = mean_goals, size=dispersion0_tmp, log=TRUE)*rep(weights,2))
    }
  } else if (model == 'gaussian'){
    if (is.null(weights)){
      sigma0_tmp <- stats::sd(all_goals)
      loglikelihood_saturated <- sum(stats::dnorm(all_goals, mean = all_goals, sd=sigma0_tmp, log=TRUE))
      loglikelihood_null <- sum(stats::dnorm(all_goals, mean = mean_goals, sd=sigma0_tmp, log=TRUE))
    } else {
      sigma0_tmp <- sqrt(sum(rep(weights, 2) * (all_goals - mean_goals)^2))
      loglikelihood_saturated <- sum(stats::dnorm(all_goals, mean = all_goals, sd=sigma0_tmp, log=TRUE)*rep(weights,2))
      loglikelihood_null <- sum(stats::dnorm(all_goals, mean = mean_goals, sd=sigma0_tmp, log=TRUE)*rep(weights,2))
    }
  } else if (model == 'cmp'){
    if (is.null(weights)){
      dispersion0_tmp <- upsilon.ml(x = all_goals, parameters = mean_goals, param_type = 'mu', method='fast')
      loglikelihood_saturated <- sum(dCMP(all_goals, lambda = lambdaCMP(all_goals, parameter_list$dispersion, method='fast'),
                                          upsilon = parameter_list$dispersion, log=TRUE))
      loglikelihood_saturated[is.nan(loglikelihood_saturated)] <- 0
      loglikelihood_null <- sum(dCMP(all_goals, lambda = lambdaCMP(mean_goals, dispersion0_tmp, method='fast'),
                                    upsilon = dispersion0_tmp, log=TRUE))
    } else {
      dispersion0_tmp <- upsilon.ml(x = all_goals, parameters = mean_goals,
                                    param_type = 'mu', method='fast', weights = rep(weights,2))
      loglikelihood_saturated <- sum(dCMP(all_goals, lambda = lambdaCMP(all_goals, parameter_list$dispersion, method='fast'),
                                          upsilon = parameter_list$dispersion, log=TRUE)*rep(weights,2))
      loglikelihood_saturated[is.nan(loglikelihood_saturated)] <- 0
      loglikelihood_null <- sum(dCMP(all_goals, lambda = lambdaCMP(mean_goals, dispersion0_tmp, method='fast'),
                                     upsilon = dispersion0_tmp, log=TRUE)*rep(weights,2))
    }
  }

  # TODO: How should deviances and R^2 be computed in the Dixon-Coles model?

  deviance <- 2 * (loglikelihood_saturated - loglikelihood)
  deviance_null <- 2 * (loglikelihood_saturated - loglikelihood_null)

  r_squared <- 1 - (deviance / deviance_null)


  # Update the all_teams variable to include teams not in data, but with
  # fixed parameters.
  all_param_teams <- unique(c(names(parameter_list$defense), names(parameter_list$attack)))
  if (any(!all_param_teams %in% all_teams)){
    all_teams <- sort(all_param_teams)
  }

  # sort the attack and defence parameters alphabetically
  parameter_list$defense <- parameter_list$defense[order(names(parameter_list$defense))]
  parameter_list$attack <- parameter_list$attack[order(names(parameter_list$attack))]


  if (any(is.na(unlist(parameter_list)))){
    wm_tmp <- 'Some parameters are NA.'
    warning_messages <- append(warning_messages, wm_tmp)
    warning(wm_tmp)
  }

  # maxgoal. Useful for later predictions.
  maxgoal <- max(all_goals)

  out <- list(parameters = parameter_list,
              loglikelihood = loglikelihood, npar_est=npar_est,
              npar_fixed = npar_fixed, aic=aic, r_squared=r_squared,
              all_teams = all_teams, ngames = ngames,
              est_time = est_time, model = model,
              fixed_params = fixed_params,
              converged = converged,
              maxgoal = maxgoal,
              fitter = fitter,
              warnings = warning_messages)

  class(out) <- 'goalmodel'

  return(out)

}

