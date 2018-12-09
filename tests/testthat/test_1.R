

library(engsoccerdata)
library(dplyr)

# Load data from English Premier League, 2011-12 season.
england %>%
  filter(Season %in% c(2011),
         tier==c(1)) %>%
  mutate(Date = as.Date(Date)) -> england_2011

context("Model fitting - Default model")

# fit default model
gm_res <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                    team1 = england_2011$home, team2=england_2011$visitor)



test_that("Fitting default model", {
  expect_equal(class(gm_res), 'goalmodel')
  expect_equal(gm_res$parameters$dispersion, NULL)
  expect_equal(gm_res$parameters$rho, NULL)
  expect_equal(gm_res$parameters$gamma, NULL)
  expect_equal(names(gm_res$parameters$attack), names(gm_res$parameters$defense))
})


context("Model fitting - DC model")


gm_res_dc <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                       team1 = england_2011$home, team2=england_2011$visitor,
                       dc=TRUE)

test_that("Fitting Dixon-Coles model", {
  expect_equal(class(gm_res_dc), 'goalmodel')
  expect_equal(gm_res_dc$parameters$dispersion, NULL)
  expect_equal(is.numeric(gm_res_dc$parameters$rho), TRUE)
  expect_equal(gm_res_dc$parameters$gamma, NULL)
  expect_equal(names(gm_res_dc$parameters$attack), names(gm_res_dc$parameters$defense))
})


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
  expect_equal(gm_res_dc_2s$parameters$gamma, NULL)
  expect_equal(names(gm_res_dc_2s$parameters$attack), names(gm_res_dc_2s$parameters$defense))
  expect_equal(gm_res_dc_2s$parameters$attack, gm_res$parameters$attack)
  expect_equal(gm_res_dc_2s$parameters$defense, gm_res$parameters$defense)
  expect_equal(gm_res_dc_2s$parameters$rho == gm_res_dc$parameters$rho, FALSE)
})




context("Model fitting - Gaussian")

# Fit a Gaussian model.
gm_res_gaussian <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                          team1 = england_2011$home, team2=england_2011$visitor, model='gaussian')

test_that("Fitting Gaussian model", {
  expect_equal(class(gm_res_gaussian), 'goalmodel')
  expect_equal(gm_res_gaussian$parameters$dispersion, NULL)
  expect_equal(is.numeric(gm_res_gaussian$parameters$sigma), TRUE)
  expect_equal(gm_res_gaussian$parameters$gamma, NULL)
  expect_equal(names(gm_res_gaussian$parameters$attack), names(gm_res_gaussian$parameters$defense))
})



context("Model fitting - Least squares")

# Fit a Gaussian model.
gm_res_ls <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                             team1 = england_2011$home, team2=england_2011$visitor, model='ls')

test_that("Fitting model with least squares", {
  expect_equal(class(gm_res_ls), 'goalmodel')
  expect_equal(gm_res_ls$parameters$dispersion, NULL)
  expect_equal(gm_res_ls$parameters$sigma, NULL)
  expect_equal(gm_res_ls$parameters$gamma, NULL)
  expect_equal(names(gm_res_ls$parameters$attack), names(gm_res_ls$parameters$defense))
})



context("Model fitting - Manually specified initial values")

my_inits <- list('defense' = c('Arsenal' = 0.8, 'Chelsea' = 0.23,
                               # Not relevant parameter, will be ignored, without warning.
                               'TeamName' = 11),
                 'hfa' = 0.31)

gm_res_inits <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                             team1 = england_2011$home, team2=england_2011$visitor,
                          initvals = my_inits)


test_that("Model fit with manual inits.", {
  expect_equal(class(gm_res_inits), 'goalmodel')
  expect_equal(gm_res_inits$parameters$dispersion, NULL)
  expect_equal(gm_res_inits$parameters$sigma, NULL)
  expect_equal(gm_res_inits$parameters$gamma, NULL)
  expect_equal(names(gm_res_inits$parameters$attack), names(gm_res_inits$parameters$defense))
  expect_equal(abs(gm_res$loglikelihood - gm_res_inits$loglikelihood) < 0.01, TRUE)
})



gm_res_inits2 <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                          team1 = england_2011$home, team2=england_2011$visitor,
                          initvals = gm_res$parameters)

test_that("Model fit with manual inits, 2 ", {
  expect_equal(class(gm_res_inits2), 'goalmodel')
  expect_equal(gm_res_inits2$parameters$dispersion, NULL)
  expect_equal(gm_res_inits2$parameters$sigma, NULL)
  expect_equal(gm_res_inits2$parameters$gamma, NULL)
  expect_equal(names(gm_res_inits2$parameters$attack), names(gm_res_inits2$parameters$defense))
  expect_equal(abs(gm_res$loglikelihood - gm_res_inits2$loglikelihood) < 0.01, TRUE)
})




context("Making predictions")

to_predict1 <- c('Arsenal', 'Manchester United')
to_predict2 <- c('Fulham', 'Chelsea')

pred_expg_default <- predict_expg(gm_res, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_expg_dc <- predict_expg(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = FALSE)

gm_res_dc0 <- gm_res_dc
gm_res_dc0$parameters$rho <- 0

pred_expg_dc0 <- predict_expg(gm_res_dc0, team1=to_predict1, team2=to_predict2, return_df = FALSE)

test_that("Predict expg.", {
  expect_equal(is.numeric(pred_expg_default[[1]]), TRUE)
  expect_equal(is.numeric(pred_expg_dc[[1]]), TRUE)
  expect_equal(is.numeric(pred_expg_dc0[[1]]), TRUE)
  expect_equal(is.numeric(pred_expg_default[[2]]), TRUE)
  expect_equal(is.numeric(pred_expg_dc[[2]]), TRUE)
  expect_equal(is.numeric(pred_expg_dc0[[2]]), TRUE)
})


pred_result_default <- predict_result(gm_res, team1=to_predict1, team2=to_predict2, return_df = FALSE)
pred_result_dc <- predict_result(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = FALSE)

test_that("Predict result", {
  expect_equal(is.numeric(pred_result_default[[1]]), TRUE)
  expect_equal(is.numeric(pred_result_dc[[1]]), TRUE)
  expect_equal(is.numeric(pred_result_default[[2]]), TRUE)
  expect_equal(is.numeric(pred_result_dc[[2]]), TRUE)
})



pred_goals_default <- predict_goals(gm_res, team1=to_predict1, team2=to_predict2)
pred_goals_dc <- predict_goals(gm_res_dc, team1=to_predict1, team2=to_predict2)

test_that("Predict goals", {
  expect_equal(is.matrix(pred_goals_default[[1]]), TRUE)
  expect_equal(is.matrix(pred_goals_dc[[1]]), TRUE)
  expect_equal(is.matrix(pred_goals_default[[2]]), TRUE)
  expect_equal(is.matrix(pred_goals_dc[[2]]), TRUE)
})


context("Misc.")

# the weighting function.
my_weights1 <- weights_dc(england_2011$Date, xi=0.0019)
my_weights2 <- weights_dc(england_2011$Date, xi=0.011)
my_weights3 <- weights_dc(england_2011$Date, xi=0.04)

test_that("The weighting function", {
  expect_equal(is.numeric(my_weights1), TRUE)
  expect_equal(is.numeric(my_weights2), TRUE)
  expect_equal(is.numeric(my_weights3), TRUE)
  expect_equal(all(my_weights3 >= 0), TRUE)
  expect_equal(all(my_weights2 >= 0), TRUE)
  expect_equal(all(my_weights3 >= 0), TRUE)
  expect_error(weights_dc(england_2011$Date, xi=-0.9), 'xi >= 0 is not TRUE')
})



