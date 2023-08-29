

require(engsoccerdata)
require(dplyr)
require(Rcpp) # Should not be necessary to load Rcpp like this, but sometimes it fails if not.

# Load data from English Premier League, 2011-12 season.
engsoccerdata::england %>%
  dplyr::filter(Season %in% c(2011),
         tier==c(1)) %>%
  dplyr::mutate(Date = as.Date(Date),
         home = as.character(home),
         visitor= as.character(visitor)) -> england_2011


# Model fitting - Default model ----
context("Model fitting - Default model")

# fit default model
gm_res <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                    team1 = england_2011$home, team2=england_2011$visitor)


test_that("Fitting default model", {
  expect_equal(class(gm_res), 'goalmodel')
  expect_equal(gm_res$parameters$dispersion, NULL)
  expect_equal(gm_res$parameters$rho, NULL)
  expect_equal(gm_res$parameters$sigma, NULL)
  expect_equal(gm_res$parameters$gamma, NULL)
  expect_equal(any(is.na(gm_res$parameters$attack)), FALSE)
  expect_equal(any(is.na(gm_res$parameters$defense)), FALSE)
  expect_equal(names(gm_res$parameters$attack), names(gm_res$parameters$defense))
  expect_equal(any(duplicated(names(gm_res$parameters$attack))), FALSE)
  expect_equal(any(duplicated(names(gm_res$parameters$defense))), FALSE)
  expect_true(gm_res$converged)
})

# Model fitting - DC model -----
context("Model fitting - DC model")


gm_res_dc <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                       team1 = england_2011$home, team2=england_2011$visitor,
                       dc=TRUE)

test_that("Fitting Dixon-Coles model", {
  expect_equal(class(gm_res_dc), 'goalmodel')
  expect_equal(gm_res_dc$parameters$dispersion, NULL)
  expect_equal(is.numeric(gm_res_dc$parameters$rho), TRUE)
  expect_equal(gm_res_dc$parameters$gamma, NULL)
  expect_equal(gm_res$parameters$sigma, NULL)
  expect_equal(any(is.na(gm_res_dc$parameters$attack)), FALSE)
  expect_equal(any(is.na(gm_res_dc$parameters$defense)), FALSE)
  expect_equal(names(gm_res_dc$parameters$attack), names(gm_res_dc$parameters$defense))
  expect_equal(any(duplicated(names(gm_res_dc$parameters$attack))), FALSE)
  expect_equal(any(duplicated(names(gm_res_dc$parameters$defense))), FALSE)
  expect_true(gm_res_dc$converged)

  # Fit DC model on dataset with where there are no low-scoring games
  expect_error(goalmodel(goals1 = england_2011$hgoal+2, goals2 = england_2011$vgoal+2,
                         team1 = england_2011$home, team2=england_2011$visitor,
                         dc=TRUE))
})


# Model fitting - Hurdle model ----
context("Model fitting - Hurdle model")

gm_res_hurdle <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                       team1 = england_2011$home, team2=england_2011$visitor,
                       hurdle=TRUE)


gm_res_hurdle$parameters$zip

test_that("Fitting Hurdle model", {
  expect_equal(class(gm_res_hurdle), 'goalmodel')
  expect_equal(gm_res_hurdle$parameters$dispersion, NULL)
  expect_equal(is.numeric(gm_res_hurdle$parameters$hurdle), TRUE)
  expect_equal(gm_res_hurdle$parameters$gamma, NULL)
  expect_equal(gm_res_hurdle$parameters$rho, NULL)
  expect_equal(gm_res$parameters$sigma, NULL)
  expect_equal(any(is.na(gm_res_hurdle$parameters$attack)), FALSE)
  expect_equal(any(is.na(gm_res_hurdle$parameters$defense)), FALSE)
  expect_equal(names(gm_res_hurdle$parameters$attack), names(gm_res_hurdle$parameters$defense))
  expect_equal(any(duplicated(names(gm_res_hurdle$parameters$attack))), FALSE)
  expect_equal(any(duplicated(names(gm_res_hurdle$parameters$defense))), FALSE)
  expect_true(gm_res_hurdle$converged)

  # Fit DC model on dataset with where there are no low-scoring games
  expect_error(goalmodel(goals1 = england_2011$hgoal+2, goals2 = england_2011$vgoal+2,
                         team1 = england_2011$home, team2=england_2011$visitor,
                         dc=TRUE))
})



# Model fitting - Negbin model ----
context("Model fitting - Negbin model")

# fit negative binomial model
gm_res_nbin <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                         team1 = england_2011$home, team2=england_2011$visitor, model='negbin')


test_that("Fitting Negative Binomial model", {
  expect_equal(class(gm_res_nbin), 'goalmodel')
  expect_true(is.null(gm_res_nbin$parameters$rho))
  expect_true(is.null(gm_res_nbin$parameters$sigma))
  expect_true(is.numeric(gm_res_nbin$parameters$dispersion))
  expect_true(length(gm_res_nbin$parameters$dispersion) == 1)
  expect_equal(any(is.na(gm_res_nbin$parameters$attack)), FALSE)
  expect_equal(any(is.na(gm_res_nbin$parameters$defense)), FALSE)
  expect_equal(names(gm_res_nbin$parameters$attack), names(gm_res_nbin$parameters$defense))
  expect_equal(any(duplicated(names(gm_res_nbin$parameters$attack))), FALSE)
  expect_equal(any(duplicated(names(gm_res_nbin$parameters$defense))), FALSE)
  expect_true(gm_res_nbin$converged)
})


# Model fitting - Gaussian ----
context("Model fitting - Gaussian")

# Fit a Gaussian model.
gm_res_gaussian <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                             team1 = england_2011$home, team2=england_2011$visitor, model='gaussian')


test_that("Fitting Gaussian model", {
  expect_equal(class(gm_res_gaussian), 'goalmodel')
  expect_equal(gm_res_gaussian$parameters$dispersion, NULL)
  expect_equal(is.numeric(gm_res_gaussian$parameters$sigma), TRUE)
  expect_equal(any(is.na(gm_res_gaussian$parameters$attack)), FALSE)
  expect_equal(any(is.na(gm_res_gaussian$parameters$defense)), FALSE)
  expect_equal(gm_res_gaussian$parameters$gamma, NULL)
  expect_equal(names(gm_res_gaussian$parameters$attack), names(gm_res_gaussian$parameters$defense))
  expect_equal(gm_res_gaussian$converged, TRUE)
})


# Model fitting - some fixed parameters ----
context("Model fitting - some fixed parameters")

my_fixed_params1 <- list(attack = c('Chelsea' = 0.2), defense= c('Fulham' = -0.09, 'Liverpool' = 0.1))

gm_res_fp1 <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                          team1 = england_2011$home, team2=england_2011$visitor,
                          fixed_params = my_fixed_params1)


# Fit model with parameter fixed for some teams not in data fixed
my_fixed_params2 <- list(attack = c('NOTEXIST' = 0.2))


test_that("Fitting default model - some parameters fixed", {

  # Fit model with parameter fixed for some teams not in data,
  # which gives a warning.

  expect_warning(gm_res_fp2 <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                                         team1 = england_2011$home, team2=england_2011$visitor,
                                         fixed_params = my_fixed_params2))

  expect_equal(class(gm_res_fp1), 'goalmodel')
  expect_equal(gm_res_fp1$parameters$dispersion, NULL)
  expect_equal(gm_res_fp1$parameters$gamma, NULL)
  expect_equal(gm_res$parameters$sigma, NULL)
  expect_equal(any(is.na(gm_res_fp1$parameters$attack)), FALSE)
  expect_equal(any(is.na(gm_res_fp1$parameters$defense)), FALSE)
  expect_equal(names(gm_res_fp1$parameters$attack), names(gm_res_fp1$parameters$defense))
  expect_equal(gm_res_fp1$parameters$attack, gm_res_fp1$parameters$attack)
  expect_equal(gm_res_fp1$parameters$defense, gm_res_fp1$parameters$defense)
  expect_equal(any(duplicated(names(gm_res_fp1$parameters$attack))), FALSE)
  expect_equal(any(duplicated(names(gm_res_fp1$parameters$defense))), FALSE)
  expect_true(gm_res_fp1$converged)

  expect_true(abs(gm_res_fp2$loglikelihood - gm_res$loglikelihood) < 0.0001)
  expect_true('NOTEXIST' %in% gm_res_fp2$all_teams)
  expect_true('NOTEXIST' %in% names(gm_res_fp2$parameters$defense))
  expect_true('NOTEXIST' %in% names(gm_res_fp2$parameters$attack))

})

# Model fitting - 2-step estimation ----
context("Model fitting - 2-step estimation")

## Test if two-step estimation works as it should.

# Fit the Dixon-Coles model, with most of the parameters fixed to the values in the default model.
gm_res_dc_2s <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                          team1 = england_2011$home, team2=england_2011$visitor,
                          dc=TRUE, fixed_params = gm_res$parameters)


test_that("Fitting Dixon-Coles model - 2step", {
  expect_equal(class(gm_res_dc_2s), 'goalmodel')
  expect_equal(gm_res_dc_2s$parameters$dispersion, NULL)
  expect_equal(is.numeric(gm_res_dc_2s$parameters$rho), TRUE)
  expect_equal(gm_res$parameters$sigma, NULL)
  expect_equal(gm_res_dc_2s$parameters$gamma, NULL)
  expect_equal(names(gm_res_dc_2s$parameters$attack), names(gm_res_dc_2s$parameters$defense))
  expect_equal(gm_res_dc_2s$parameters$attack, gm_res$parameters$attack)
  expect_equal(gm_res_dc_2s$parameters$defense, gm_res$parameters$defense)
  expect_equal(any(is.na(gm_res_dc_2s$parameters$attack)), FALSE)
  expect_equal(any(is.na(gm_res_dc_2s$parameters$defense)), FALSE)
  expect_equal(gm_res_dc_2s$parameters$rho == gm_res_dc$parameters$rho, FALSE)
  expect_equal(any(duplicated(names(gm_res_dc_2s$parameters$attack))), FALSE)
  expect_equal(any(duplicated(names(gm_res_dc_2s$parameters$defense))), FALSE)
  expect_true(gm_res_dc_2s$converged)
})


# Additional covariates ----
context("Additional covariates")


# Manual hfa
hfa_mat <- matrix(data = 1, ncol=1, nrow=nrow(england_2011))
colnames(hfa_mat) <- 'hfaa'


gm_res_hfax <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                          team1 = england_2011$home, team2=england_2011$visitor,
                          hfa=FALSE, x1=hfa_mat)

test_that("Manual HFA", {
  expect_equal(names(gm_res_hfax$parameters$beta), 'hfaa')
  expect_true(abs(gm_res_hfax$parameters$beta - gm_res$parameters$hfa) < 0.001)
  expect_true(gm_res_hfax$converged)
})



# CMP functions ----
context("CMP functions")

dcmp_vec <- dCMP(x=0:6, lambda=4.4, upsilon = 1.2)
pcmp_vec <- pCMP(q=6, lambda=4.4, upsilon = 1.2)
dp_diff <- sum(dcmp_vec) - pcmp_vec

dcmp_log_vec <- dCMP(x=0:6, lambda=4.4, upsilon = 1.2, log = TRUE)

dcmp_vec2 <- dCMP(x=c(0,NA,2,3), lambda=4.4, upsilon = 1.2)
pcmp_vec2 <- pCMP(q=c(0,NA,2,3), lambda=c(1.2, NA), upsilon = 0.98)

qCMP_vec <- qCMP(p = seq(0,1, by=0.05), lambda=2.2, upsilon=1)
qpois_vec <- qpois(p = seq(0,1, by=0.05), lambda=2.2)


test_that("CMP", {
  expect_true(all(dcmp_vec >= 0))
  expect_true(all(dcmp_vec <= 1))
  expect_true(pcmp_vec >= 0)
  expect_true(pcmp_vec <= 1)
  expect_true(dp_diff < 0.0001)
  expect_true(all(dcmp_log_vec == log(dcmp_vec)))
  expect_true(is.na(dcmp_vec2[2]))
  expect_true(all(!is.na(dcmp_vec2[-2])))
  expect_true(all(is.na(pcmp_vec2[c(2,4)])))
  expect_true(all(!is.na(pcmp_vec2[-c(2,4)])))
  expect_true(all(qCMP_vec == qpois_vec))
})



# Model fitting - CMP 2 - step ----
context("Model fitting - CMP 2 - step")


# fit CMP model
gm_res_cmp <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                    team1 = england_2011$home, team2=england_2011$visitor,
                    fixed_params = gm_res$parameters,
                    model='cmp')


# Estiamte the dispersion with the upsilon.ml function.
expg_default <- predict_expg(gm_res, team1 = england_2011$home, team2=england_2011$visitor)

upsilon_est <- upsilon.ml(x = c(england_2011$hgoal, england_2011$vgoal),
                          parameters=c(expg_default$expg1, expg_default$expg2),
                          param_type = 'mu', method='fast')

upsilon_diff <- abs(gm_res_cmp$parameters$dispersion - upsilon_est)


test_that("Fitting CMP model", {
  expect_equal(class(gm_res_cmp), 'goalmodel')
  expect_true(is.null(gm_res_cmp$parameters$rho))
  expect_true(is.null(gm_res_cmp$parameters$sigma))
  expect_true(is.numeric(gm_res_cmp$parameters$dispersion))
  expect_true(length(gm_res_cmp$parameters$dispersion) == 1)
  expect_equal(any(is.na(gm_res_cmp$parameters$attack)), FALSE)
  expect_equal(any(is.na(gm_res_cmp$parameters$defense)), FALSE)
  expect_equal(names(gm_res_cmp$parameters$attack), names(gm_res_cmp$parameters$defense))
  expect_equal(any(duplicated(names(gm_res_cmp$parameters$attack))), FALSE)
  expect_equal(any(duplicated(names(gm_res_cmp$parameters$defense))), FALSE)
  expect_true(gm_res_cmp$converged)
  expect_true(upsilon_diff < 0.001)
})



# Making predictions ----
context("Making predictions")

to_predict1 <- c('Arsenal', 'Manchester United', 'Liverpool', 'Stoke City')
to_predict2 <- c('Fulham', 'Chelsea', 'Tottenham Hotspur', 'Manchester City')

pred_expg_default <- predict_expg(gm_res, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_expg_dc <- predict_expg(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_expg_nbin <- predict_expg(gm_res_nbin, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_expg_cmp <- predict_expg(gm_res_cmp, team1=to_predict1, team2=to_predict2, return_df = FALSE)

all(sapply(pred_expg_cmp, function(x) all(!is.na(x))))

gm_res_dc0 <- gm_res_dc
gm_res_dc0$parameters$rho <- 0

pred_expg_dc0 <- predict_expg(gm_res_dc0, team1=to_predict1, team2=to_predict2, return_df = FALSE)


test_that("Predict expg.", {
  expect_equal(is.numeric(pred_expg_default[[1]]), TRUE)
  expect_equal(is.numeric(pred_expg_dc[[1]]), TRUE)
  expect_equal(is.numeric(pred_expg_nbin[[1]]), TRUE)
  expect_equal(is.numeric(pred_expg_cmp[[1]]), TRUE)
  expect_equal(is.numeric(pred_expg_dc0[[1]]), TRUE)

  expect_equal(is.numeric(pred_expg_default[[2]]), TRUE)
  expect_equal(is.numeric(pred_expg_dc[[2]]), TRUE)
  expect_equal(is.numeric(pred_expg_nbin[[2]]), TRUE)
  expect_equal(is.numeric(pred_expg_cmp[[2]]), TRUE)
  expect_equal(is.numeric(pred_expg_dc0[[2]]), TRUE)

  all(sapply(pred_expg_default, function(x) all(!is.na(x))))
  all(sapply(pred_expg_dc, function(x) all(!is.na(x))))
  all(sapply(pred_expg_nbin, function(x) all(!is.na(x))))
  all(sapply(pred_expg_cmp, function(x) all(!is.na(x))))
  all(sapply(pred_expg_dc0, function(x) all(!is.na(x))))

})


# Need to supress warnings here, otherwise the tests will not pass.
suppressWarnings({

  ## Copy pasta from the "some parameters fixed" tests.
  gm_res_fp2 <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                          team1 = england_2011$home, team2=england_2011$visitor,
                          fixed_params = my_fixed_params2)

  # predict with team not in data
  p_expg_na1 <- predict_expg(gm_res, team1=c('NOTEXIST', 'Fulham', 'Fulham'),
                             team2=c('Fulham', 'NOTEXIST2', 'Arsenal'), return_df = FALSE)


  p_expg_na2 <- predict_expg(gm_res_fp2, team1=c('NOTEXIST'),
                             team2=c('Fulham'), return_df = FALSE)


  p_goals_na <- predict_goals(gm_res_fp2, team1=c('NOTEXIST', 'Fulham'),
                              team2=c('Fulham', 'Arsenal'))

  p_result_na <- predict_result(gm_res_fp2, team1=c('NOTEXIST', 'Fulham'),
                                team2=c('Fulham', 'Arsenal'))


})



test_that("Predict expg unknwon teams", {

  expect_true(is.na(p_expg_na2$expg2[1]))
  expect_true(!is.na(p_expg_na2$expg1[1]))

  expect_true(all(is.na(p_goals_na[[1]])))
  expect_true(all(!is.na(p_goals_na[[2]])))

  expect_true(all(is.na(p_result_na[1,])))
  expect_true(all(!is.na(p_result_na[2,])))

})


pred_result_default <- predict_result(gm_res, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_result_dc <- predict_result(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_result_nbin <- predict_result(gm_res_nbin, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_result_cmp <- predict_result(gm_res_cmp, team1=to_predict1, team2=to_predict2, return_df = FALSE)


test_that("Predict result", {
  expect_equal(is.numeric(pred_result_default[[1]]), TRUE)
  expect_equal(is.numeric(pred_result_dc[[1]]), TRUE)
  expect_equal(is.numeric(pred_result_nbin[[1]]), TRUE)
  expect_equal(is.numeric(pred_result_cmp[[1]]), TRUE)

  expect_equal(is.numeric(pred_result_default[[2]]), TRUE)
  expect_equal(is.numeric(pred_result_dc[[2]]), TRUE)
  expect_equal(is.numeric(pred_result_nbin[[2]]), TRUE)
  expect_equal(is.numeric(pred_result_cmp[[2]]), TRUE)

  expect_true(all(rowSums(pred_result_default) == 1))
  expect_true(all(rowSums(pred_result_dc) == 1))
  expect_true(all(rowSums(pred_result_nbin) == 1))
  expect_true(all(rowSums(pred_result_cmp) == 1))
})



pred_goals_default <- predict_goals(gm_res, team1=to_predict1, team2=to_predict2)
pred_goals_dc <- predict_goals(gm_res_dc, team1=to_predict1, team2=to_predict2)

pred_goals_default_df <- predict_goals(gm_res, team1=to_predict1, team2=to_predict2, return_df = TRUE)
pred_goals_dc_df <- predict_goals(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = TRUE)


test_that("Predict goals", {
  expect_equal(is.matrix(pred_goals_default[[1]]), TRUE)
  expect_equal(is.matrix(pred_goals_dc[[1]]), TRUE)
  expect_equal(is.matrix(pred_goals_default[[2]]), TRUE)
  expect_equal(is.matrix(pred_goals_dc[[2]]), TRUE)
  expect_equal(any(is.na(pred_goals_default[[1]])), FALSE)
  expect_equal(any(is.na(pred_goals_dc[[1]])), FALSE)
  expect_equal(any(is.na(pred_goals_default[[2]])), FALSE)
  expect_equal(any(is.na(pred_goals_dc[[2]])), FALSE)

  expect_equal(is.data.frame(pred_goals_default_df), TRUE)
  expect_equal(is.data.frame(pred_goals_dc_df), TRUE)

  # The sum of probabilites should be the number of matches predicted
  # (ie the probabilities for each match should sum to 1)
  expect_true(abs(sum(pred_goals_default_df$probability) - length(to_predict1)) <= 0.001)
  expect_true(abs(sum(pred_goals_dc_df$probability) - length(to_predict1)) <= 0.001)

})


pred_ou_default <- predict_ou(gm_res, team1=to_predict1, team2=to_predict2)
pred_ou_dc <- predict_ou(gm_res_dc, team1=to_predict1, team2=to_predict2)

pred_ou_default_df <- predict_ou(gm_res, team1=to_predict1, team2=to_predict2, return_df = TRUE)
pred_ou_dc_df <- predict_ou(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = TRUE)

test_that("Predict ou", {
  pred_ou_default

  expect_true(is.list(pred_ou_default))
  expect_true(is.list(pred_ou_dc))

  expect_true(all(abs(pred_ou_default$prob_under + pred_ou_default$prob_over - 1) <= 0.0001))
  expect_true(all(abs(pred_ou_dc$prob_under + pred_ou_dc$prob_over - 1) <= 0.0001))

  expect_equal(is.data.frame(pred_ou_default_df), TRUE)
  expect_equal(is.data.frame(pred_ou_dc_df), TRUE)

})


pred_btts_default <- predict_btts(gm_res, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_btts_dc <- predict_btts(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_btts_nbin <- predict_btts(gm_res_nbin, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_btts_cmp <- predict_btts(gm_res_cmp, team1=to_predict1, team2=to_predict2, return_df = FALSE)

pred_btts_default_df <- predict_btts(gm_res, team1=to_predict1, team2=to_predict2, return_df = TRUE)


test_that("Predict btts", {

  expect_true(is.list(pred_btts_default))
  expect_true(is.list(pred_btts_dc))
  expect_true(is.list(pred_btts_nbin))
  expect_true(is.list(pred_btts_cmp))

  expect_true(all(abs(pred_btts_default$prob_btts >= 0)))
  expect_true(all(abs(pred_btts_dc$prob_btts >= 0)))
  expect_true(all(abs(pred_btts_nbin$prob_btts >= 0)))
  expect_true(all(abs(pred_btts_cmp$prob_btts >= 0)))

  expect_true(all(abs(pred_btts_default$prob_btts <= 1)))
  expect_true(all(abs(pred_btts_dc$prob_btts <= 1)))
  expect_true(all(abs(pred_btts_nbin$prob_btts <= 1)))
  expect_true(all(abs(pred_btts_cmp$prob_btts <= 1)))

  expect_true(is.data.frame(pred_btts_default_df))


})



# DC weight function----
context("DC weights")

# the weighting function.
my_weights1 <- weights_dc(england_2011$Date, xi=0.0019)
my_weights2 <- weights_dc(england_2011$Date, xi=0.011)
my_weights3 <- weights_dc(england_2011$Date, xi=0.04)

gm_res_w <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                     team1 = england_2011$home, team2=england_2011$visitor,
                     weights = my_weights1)


test_that("The weighting function", {
  expect_equal(is.numeric(my_weights1), TRUE)
  expect_equal(is.numeric(my_weights2), TRUE)
  expect_equal(is.numeric(my_weights3), TRUE)
  expect_equal(all(my_weights3 >= 0), TRUE)
  expect_equal(all(my_weights2 >= 0), TRUE)
  expect_equal(all(my_weights3 >= 0), TRUE)
  expect_equal(all(unlist(gm_res_w$parameters) == unlist(gm_res$parameters)), FALSE)
  expect_equal(gm_res_w$converged, TRUE)
  expect_error(weights_dc(england_2011$Date, xi=-0.9), 'xi >= 0 is not TRUE')

})



# dDCP function ----
context("dDCP function")


# Sum to 1.
dcp_sum <- sum(dDCP(x1=rep(0:15,16),
                x2=rep(0:15,each=16), 0.5, 1.2))

# Check that it equals independent Poisson where it should
dcp_test1 <- dDCP(x1=3, x2=2, 0.5, 1.2, rho=0.01) == dpois(3, lambda=0.5) * dpois(2, lambda=1.2)
dcp_test2 <- dDCP(x1=1, x2=2:5, 0.5, 1.2, rho=0.01) == dpois(1, lambda=0.5) * dpois(2:5, lambda=1.2)

dcp_test3 <- dDCP(x1=3, x2=2, 0.5, 1.2, rho=-0.02) == dpois(3, lambda=0.5) * dpois(2, lambda=1.2)
dcp_test4 <- dDCP(x1=1, x2=2:5, 0.5, 1.2, rho=-0.02) == dpois(1, lambda=0.5) * dpois(2:5, lambda=1.2)

# Check that it is different from the independent Poisson where it should
dcp_test5 <- dDCP(x1=1, x2=0:1, 0.5, 1.2, rho=0.01) != dpois(1, lambda=0.5) * dpois(0:1, lambda=1.2)


# Compare dDCP with the result from predict_goals using a fitted DC model.

# Only look athe first game.
to_test_idx <- pred_goals_dc_df$team1 == 'Arsenal'

# Compute the probabilities using dDCP.
dc_prob2 <- dDCP(x1 = pred_goals_dc_df$goals1[to_test_idx], x2 = pred_goals_dc_df$goals2[to_test_idx],
                lambda1 = pred_expg_dc$expg1[1], lambda2 = pred_expg_dc$expg2[1],
                rho = gm_res_dc$parameters$rho)

# Add to data.frame,
pred_goals_dc_df_subset <- pred_goals_dc_df[to_test_idx,]
pred_goals_dc_df_subset$probability2 <- dc_prob2

# Compare
dcp_test6 <- all(abs(pred_goals_dc_df_subset$probability - pred_goals_dc_df_subset$probability2) < 0.00001)


test_that('Dixon-Coles probability function', {
  expect_true(abs(dcp_sum - 1) < 0.00001)
  expect_true(dcp_test1)
  expect_true(all(dcp_test2))
  expect_true(dcp_test3)
  expect_true(all(dcp_test4))
  expect_true(all(dcp_test5))
  expect_true(all(dcp_test6))
})




# expg_from_probabilities ----
context("expg_from_probabilities")


# Compute probabilities from the default model.
pred_result_default_all <- predict_result(gm_res, team1 = england_2011$home,
                                          team2=england_2011$visitor, return_df = FALSE)


pred_result_dc_all <- predict_result(gm_res_dc, team1 = england_2011$home,
                                      team2=england_2011$visitor, return_df = FALSE)

# Also compute expected goals, as a comparison.
pred_result_expg_all <- predict_expg(gm_res, team1 = england_2011$home,
                                     team2=england_2011$visitor, return_df = FALSE)

# Also compute expected goals, as a comparison.
pred_result_expg_dc_all <- predict_expg(gm_res_dc, team1 = england_2011$home,
                                     team2=england_2011$visitor, return_df = FALSE)

# Reverse-engineer the expg from the probabilities.
expgfp <- expg_from_probabilities(pred_result_default_all, uprx = 30)

expgfpdc <- expg_from_probabilities(pred_result_dc_all,
                                    rho=gm_res_dc$parameters$rho, uprx = 30)

# hist(pred_result_expg_all$expg1 - expgfp$expg[,1])
#
# hist(pred_result_expg_dc_all$expg1 - expgfpdc$expg[,1])
# hist(pred_result_expg_dc_all$expg2 - expgfpdc$expg[,2])

test_that("expg_from_probabilities", {

  # Default
  expect_true(length(expgfp) == 2)
  expect_true(ncol(expgfp$expg) == 2)
  expect_false(any(is.na(expgfp$expg)))
  expect_false(any(is.na(expgfp$sq_errors)))

  expect_true(all(abs(pred_result_expg_all$expg1 - expgfp$expg[,1]) < 0.01))
  expect_true(all(abs(pred_result_expg_all$expg2 - expgfp$expg[,2]) < 0.01))

  # DC
  expect_true(length(expgfpdc) == 2)
  expect_true(ncol(expgfpdc$expg) == 2)
  expect_false(any(is.na(expgfpdc$expg)))
  expect_false(any(is.na(expgfpdc$sq_errors)))

   expect_true(all(abs(pred_result_expg_dc_all$expg1 - expgfpdc$expg[,1]) < 0.01))
   expect_true(all(abs(pred_result_expg_dc_all$expg2 - expgfpdc$expg[,2]) < 0.01))
})


# Warnings ----
context("Warnings")

# Add a disconected fake data to test warning.
england_2011_tmp <- bind_rows(england_2011,
                              data.frame(home=c('ff', 'aaa'), visitor=c('aaa', 'zzz'),
                                         hgoal=c(1,1), vgoal=c(1,1), stringsAsFactors = FALSE))

test_that("Warning messages during model fitting", {
  expect_warning(goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                           team1 = england_2011$home, team2=england_2011$visitor,
                           fixed_params = list(dispersion=1)))

  expect_warning(goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                           team1 = england_2011$home, team2=england_2011$visitor,
                           fixed_params = list(dispersion=1), model='gaussian'))

  expect_warning(goalmodel(goals1 = england_2011_tmp$hgoal, goals2 = england_2011_tmp$vgoal,
                           team1 = england_2011_tmp$home, team2=england_2011_tmp$visitor))

})

# Match schedule functions ----
context("Match schedule functions")


dslm <- days_since_last_match(england_2011$home, england_2011$visitor, england_2011$Date)

test_that("days_since_last_match", {

  expect_true(ncol(dslm) == 2)
  expect_true(nrow(dslm) == nrow(england_2011))
  expect_true(sum(is.na(dslm)) == length(gm_res$all_teams))

})

mlxd <- matches_last_xdays(england_2011$home, england_2011$visitor, england_2011$Date)

test_that("matches_last_xdays", {

  expect_true(ncol(mlxd) == 2)
  expect_true(nrow(mlxd) == nrow(england_2011))
  expect_true(sum(is.na(mlxd)) == length(gm_res$all_teams))

})


# p1x2 function ----
context("p1x2 function")


# Check that p1x2() gives same predictions as predict_result gives.
p1x2_res_default <- p1x2(expg1 = pred_expg_default$expg1, expg2 = pred_expg_default$expg2,
                         model = 'poisson')

p1x2_res_dc <- p1x2(expg1 = pred_expg_dc$expg1, expg2 = pred_expg_dc$expg2,
                    model = 'poisson', rho = gm_res_dc$parameters$rho)

p1x2_res_nbin <- p1x2(expg1 = pred_expg_cmp$expg1, expg2 = pred_expg_cmp$expg2,
                     model = 'negbin', dispersion = gm_res_nbin$parameters$dispersion)

p1x2_res_cmp <- p1x2(expg1 = pred_expg_cmp$expg1, expg2 = pred_expg_cmp$expg2,
                model = 'cmp', dispersion = gm_res_cmp$parameters$dispersion)


test_that("p1x2", {
  expect_true(all(abs(p1x2_res_default - pred_result_default) <= 0.0001))
  expect_true(all(abs(p1x2_res_dc - pred_result_dc) <= 0.0001))
  expect_true(all(abs(p1x2_res_cmp - p1x2_res_cmp) <= 0.0001))
  expect_true(all(abs(p1x2_res_nbin - p1x2_res_nbin) <= 0.0001))
})


# pbtts function ----
context("pbtts function")

pbtts_res_default <- pbtts(expg1 = pred_expg_default$expg1,
                           expg2 = pred_expg_default$expg2,
                           model = 'poisson')


test_that("pbtts", {
  expect_true(is.numeric(pbtts_res_default))
  expect_true(all(abs(pbtts_res_default - pred_btts_default$prob_btts) <= 0.0001))
  expect_true(length(pbtts_res_default) == length(to_predict1))
})



# score_predictions function ----
context("score_predictions function")

observed_hda_vec <- match(as.character(england_2011$result), c('H', 'D', 'A'))

scores_result_default <- score_predictions(predictions = pred_result_default_all,
                                      observed = observed_hda_vec,
                                      score = c('log', 'brier', 'rps'))



scores_result_dc <- score_predictions(predictions = pred_result_dc_all,
                                      observed = observed_hda_vec,
                                      score = c('log', 'brier', 'rps'))

test_that("score_predictions", {
  expect_true(length(scores_result_default) == 3)
  expect_true(length(scores_result_dc) == 3)

  expect_true(length(scores_result_default$log) == nrow(pred_result_default_all))
  expect_true(length(scores_result_dc$log) == nrow(pred_result_default_all))

  expect_true(length(scores_result_default$brier) == nrow(pred_result_default_all))
  expect_true(length(scores_result_dc$brier) == nrow(pred_result_default_all))

  expect_true(length(scores_result_default$rps) == nrow(pred_result_default_all))
  expect_true(length(scores_result_dc$rps) == nrow(pred_result_default_all))

  expect_true(all(scores_result_default$log > 0))
  expect_true(all(scores_result_default$brier > 0))
  expect_true(all(scores_result_default$rps > 0))

  expect_true(all(scores_result_default$brier < 2))
  expect_true(all(scores_result_default$rps < 1))

  expect_true(all(scores_result_dc$log > 0))
  expect_true(all(scores_result_dc$brier > 0))
  expect_true(all(scores_result_dc$rps > 0))

  expect_true(all(scores_result_dc$brier < 2))
  expect_true(all(scores_result_dc$rps < 1))


})








