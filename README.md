<!-- README.md is generated from README.Rmd. Please edit that file -->
The goalmodel package let you model the number of goals scored in sport games. The models are primarily aimed at modelling and predicting football (soccer) scores, but could also be applicable for similar sports, such as hockey and handball.

Installation
============

``` r
install.packages("devtools")
devtools::install_github("opisthokonta/goalmodel")
```

The default model
=================

The goalmodel package models the goal scoring intensity so that it is a function of the two opposing teams attack and defense ratings. Let λ<sub>1</sub> and λ<sub>2</sub>) be the goal scoring intensities for the two sides. The default model in the goalmodel package then models these as

log(λ<sub>1</sub>)= μ + hfa+attack<sub>1</sub> − defense<sub>2</sub>

log(λ<sub>2</sub>)= μ + attack<sub>2</sub> − defense<sub>1</sub>

where μ is the intercept (approximately the average number of goals scored) and hfa is the home field advantage given to team 1. By default the number of goals scored, Y<sub>1</sub> and Y<sub>2</sub> are distributed as

Y<sub>1</sub> ∼ Poisson(λ<sub>1</sub>)

Y<sub>2</sub> ∼ Poisson(λ<sub>2</sub>)

The default model can be modified in numerous ways as detailed in this vignette.

Fitting the default model
=========================

Models are fitted with the goalmodel function. The minimum required input is vectors containing the number of goals scored and the names of the teams. Here I use data from the excellent [engsoccerdata](https://cran.r-project.org/web/packages/engsoccerdata/) package.

``` r
library(goalmodel)
library(dplyr) # Useful for data manipulation. 
library(engsoccerdata) # Some data.

# Load data from English Premier League, 2011-12 season.
england %>% 
  filter(Season %in% c(2011),
         tier==c(1)) %>% 
  mutate(Date = as.Date(Date)) -> england_2011


# Fit the default model, with the home team as team1.

gm_res <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                     team1 = england_2011$home, team2=england_2011$visitor)

# Show the estimated attack and defense ratings and more.
summary(gm_res)
```

    ## Model sucsessfully fitted in 1.06 seconds
    ## 
    ## Number of matches           380 
    ## Number of teams              20 
    ## 
    ## Model                     Poisson 
    ## 
    ## Log Likelihood            -1088.99 
    ## AIC                        2259.98 
    ## R-squared                  0.18 
    ## Parameters (estimated)       41 
    ## Parameters (fixed)            0 
    ## 
    ## Team                      Attack   Defense
    ## Arsenal                    0.30     0.03 
    ## Aston Villa               -0.39    -0.01 
    ## Blackburn Rovers          -0.10    -0.41 
    ## Bolton Wanderers          -0.15    -0.40 
    ## Chelsea                    0.17     0.10 
    ## Everton                   -0.10     0.26 
    ## Fulham                    -0.13     0.01 
    ## Liverpool                 -0.16     0.26 
    ## Manchester City            0.51     0.53 
    ## Manchester United          0.47     0.41 
    ## Newcastle United           0.02     0.01 
    ## Norwich City              -0.04    -0.25 
    ## Queens Park Rangers       -0.23    -0.24 
    ## Stoke City                -0.42    -0.01 
    ## Sunderland                -0.20     0.12 
    ## Swansea City              -0.22     0.02 
    ## Tottenham Hotspur          0.18     0.21 
    ## West Bromwich Albion      -0.19    -0.00 
    ## Wigan Athletic            -0.25    -0.18 
    ## Wolverhampton Wanderers   -0.28    -0.45 
    ## -------
    ## Intercept                  0.19 
    ## Home field advantage       0.27

The Dixon-Coles model
=====================

The model in the paper by Dixon and Coles (1997) they introduce a model modifies the probabilities for low scores, where each team scores at most 1 goal each. The amount of adjustment is determined by the parameter ρ (rho), which can be estimated from the data. The Dixon-Coles model can be fitted by setting the dc argument to TRUE.

``` r
# Fit the Dixon-Coles model.

gm_res_dc <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                     team1 = england_2011$home, team2=england_2011$visitor,
                     dc=TRUE)

summary(gm_res_dc)
```

    ## Model sucsessfully fitted in 1.19 seconds
    ## 
    ## Number of matches           380 
    ## Number of teams              20 
    ## 
    ## Model                     Poisson 
    ## 
    ## Log Likelihood            -1087.36 
    ## AIC                        2258.72 
    ## R-squared                  0.18 
    ## Parameters (estimated)       42 
    ## Parameters (fixed)            0 
    ## 
    ## Team                      Attack   Defense
    ## Arsenal                    0.31     0.03 
    ## Aston Villa               -0.37    -0.03 
    ## Blackburn Rovers          -0.12    -0.41 
    ## Bolton Wanderers          -0.14    -0.40 
    ## Chelsea                    0.17     0.09 
    ## Everton                   -0.12     0.27 
    ## Fulham                    -0.13     0.01 
    ## Liverpool                 -0.17     0.25 
    ## Manchester City            0.50     0.55 
    ## Manchester United          0.46     0.43 
    ## Newcastle United           0.03     0.00 
    ## Norwich City              -0.04    -0.25 
    ## Queens Park Rangers       -0.24    -0.23 
    ## Stoke City                -0.42    -0.01 
    ## Sunderland                -0.20     0.12 
    ## Swansea City              -0.21     0.01 
    ## Tottenham Hotspur          0.18     0.21 
    ## West Bromwich Albion      -0.20     0.00 
    ## Wigan Athletic            -0.25    -0.17 
    ## Wolverhampton Wanderers   -0.27    -0.46 
    ## -------
    ## Intercept                  0.18 
    ## Home field advantage       0.27 
    ## Dixon-Coles adj. (rho)    -0.13

Notice that the estimated ρ parameter is listed together with the other parameters.

The Rue-Salvesen adjustment
===========================

In a paper by Rue and Salvesen (2001) they introduce an adjustment to the goals scoring intesities λ<sub>1</sub> and λ<sub>2</sub>:

log(λ<sub>1</sub><sup>adj</sup>)=log(λ<sub>1</sub>)−γΔ<sub>1,2</sub>

log(λ<sub>2</sub><sup>adj</sup>)=log(λ<sub>2</sub>)+γΔ<sub>1,2</sub>

where

Δ<sub>1,2</sub> = (attack<sub>1</sub>+defense<sub>1</sub>−attack<sub>2</sub>−defense<sub>2</sub>)/2

and γ is a parameter that modifies the amount of adjustment. This model can be fitted by setting the rs argument to TRUE.

``` r
# Fit the model with the Rue-Salvesen adjustment.

gm_res_rs <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                     team1 = england_2011$home, team2=england_2011$visitor,
                     rs=TRUE)

summary(gm_res_rs)
```

    ## Model sucsessfully fitted in 1.37 seconds
    ## 
    ## Number of matches           380 
    ## Number of teams              20 
    ## 
    ## Model                     Poisson 
    ## 
    ## Log Likelihood            -1088.99 
    ## AIC                        2261.98 
    ## R-squared                  0.18 
    ## Parameters (estimated)       42 
    ## Parameters (fixed)            0 
    ## 
    ## Team                      Attack   Defense
    ## Arsenal                    0.31     0.04 
    ## Aston Villa               -0.40    -0.02 
    ## Blackburn Rovers          -0.12    -0.43 
    ## Bolton Wanderers          -0.16    -0.41 
    ## Chelsea                    0.18     0.11 
    ## Everton                   -0.09     0.26 
    ## Fulham                    -0.13     0.01 
    ## Liverpool                 -0.16     0.26 
    ## Manchester City            0.54     0.57 
    ## Manchester United          0.50     0.44 
    ## Newcastle United           0.03     0.01 
    ## Norwich City              -0.04    -0.26 
    ## Queens Park Rangers       -0.24    -0.25 
    ## Stoke City                -0.43    -0.02 
    ## Sunderland                -0.20     0.12 
    ## Swansea City              -0.22     0.01 
    ## Tottenham Hotspur          0.19     0.23 
    ## West Bromwich Albion      -0.20    -0.01 
    ## Wigan Athletic            -0.27    -0.19 
    ## Wolverhampton Wanderers   -0.30    -0.47 
    ## -------
    ## Intercept                  0.19 
    ## Home field advantage       0.27 
    ## Rue-Salvesen adj. (gamma)  0.06

Making predictions
==================

There is little use in fitting the model without making predictions. Several functions are provided that make different types of predictions.

We can get the expected goals with the predict\_expg function. Here the return\_df is set to TRUE, which returns a convenient data.frame.

``` r
to_predict1 <- c('Arsenal', 'Manchester United', 'Bolton Wanderers')
to_predict2 <- c('Fulham', 'Chelsea', 'Liverpool')

predict_expg(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = TRUE)
```

    ##                               team1     team2    expg1     expg2
    ## Arsenal                     Arsenal    Fulham 2.119342 1.0249899
    ## Manchester United Manchester United   Chelsea 2.291206 0.9289649
    ## Bolton Wanderers   Bolton Wanderers Liverpool 1.069256 1.5093093

We can also get the probabilities of the final outcome (team1 win, draw, team2 win) with the predict\_result function.

``` r
predict_result(gm_res_dc, team1=to_predict1, team2=to_predict2, return_df = TRUE)
```

    ##               team1     team2        p1        pd        p2
    ## 1           Arsenal    Fulham 0.6119676 0.2266340 0.1613984
    ## 2 Manchester United   Chelsea 0.6684588 0.2051687 0.1263725
    ## 3  Bolton Wanderers Liverpool 0.2522542 0.2902563 0.4574895

Other functions are predict\_goals which return matrices of the probabilities of each scoreline, and predict\_ou for getting the probabilities for over/under total scores.

Weights
=======

For predicton purposes it is a good idea to weight the influence of each match on the estimates so that matches far back in time have less influence than the more recent ones. The Dixon-Coles paper describe a function that can help set the weights. This function is implemented in the weights\_dc function. The degree more recent matches are weighted relative to earlier ones are determined by the parameter xi. Good values of xi is usually somewhere between 0.001 and 0.003

Here we calculate the weights, plot them, and fit the default model with the weights.

``` r
my_weights <- weights_dc(england_2011$Date, xi=0.0019)

length(my_weights)
```

    ## [1] 380

``` r
plot(england_2011$Date, my_weights)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
gm_res_w <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal, 
                     team1 = england_2011$home, team2=england_2011$visitor,
                     weights = my_weights)
```

Additional covariates
=====================

It is possible to use additional variables to predict the outcome. One particular useful thing this can be used for is to have greater control of the home field advantage. For example if you use data from several leagues you might want to have a separate home field advantage for each league, or be able to turn of the advantage for games played on neutral ground.

Here is how to specify the default model with the home field advantage specified as additional covariates. Notice how the hfa argument is set to FALSE, so that the default home field advantage factor is droped. Also notice that only covariates for the first team is given, implicitly setting all values of this covariate to 0 for team2.

``` r
# Create a n-by-1 matrix, containing only 1's, to indicate that all 
# games have a home team.
xx1_hfa <- matrix(1, nrow=nrow(england_2011), ncol=1)
colnames(xx1_hfa) <- 'hfa' # must be named.

# Fit the default model, with the home team as team1.
gm_res_hfa <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                     team1 = england_2011$home, team2=england_2011$visitor,
                    hfa = FALSE, # Turn of the built-in Home Field Advantage
                    x1 = xx1_hfa) # Provide the covariate matrix.

# the coefficients for the additional covariates are found in the 'beta' element in 
# the parameter list. They will also show up in the summary.
gm_res_hfa$parameters$beta
```

    ##       hfa 
    ## 0.2679329

The Negative Binomial model
===========================

All other models in this vignette has assumed that the number of goals scored are distributed according to the Poisson distribution. The Poisson distribution has a lot of nice properties, but also some limitations. For inctance there is only one parameter that describes both the expected value (the average) and the variance. The Negative Binomial (NB) model is a more flexible model that allows higher variance than the Poisson mode. The NB model introduces another parameter called the "dispersion" parameter. This parameter has to be positive, and the greater the value of this parameter, the more variance there is relative to the Poisson model with the same *λ* (called overdispersion). When the parameter is close to zero, it the result is approximately the Poisson. The NB model can be fitted by setting the "model" argument to 'negbin'. In this example the dispersion parameter is close to zero.

``` r
# Fit the model with overdispersion.

gm_res_nb <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                     team1 = england_2011$home, team2=england_2011$visitor,
                     model='negbin')


gm_res_nb$parameters$dispersion
```

    ## [1] 4.333503e-05

Fixed parameters - Two step estimation.
=======================================

If you want to, you can set some of the parameters to a fixed value that is kept constant during model fitting. In other words, you don't estimate the fixed parameters from the data, but set them manually beforehand and all other parameters are estimated with this in mind.

This feature can be useful for doing two-step estimation. Sometimes the model fitting can be unstable, especially when the model is complicated. It can then be a good idea to first fit a simpler model, like the default model, and then with the parameters from the simpler model kept constant, estimate the rest of the parameters in a second step.

Here is an example of two-step fitting of the Dixon-Coles model.

``` r
# Fit the Dixon-Coles model, with most of the parameters fixed to the values in the default model.
gm_res_dc_2s <- goalmodel(goals1 = england_2011$hgoal, goals2 = england_2011$vgoal,
                     team1 = england_2011$home, team2=england_2011$visitor, 
                     dc=TRUE, fixed_params = gm_res$parameters)

# Compare the two estimates of rho. They are pretty similar. 
gm_res_dc_2s$parameters$rho
```

    ## [1] -0.1261845

``` r
gm_res_dc$parameters$rho
```

    ## [1] -0.1334634

Modifying parameters
====================

In addition to fixing parameters before the model is fit, you can also manually set the parameters after the model is fitted. This can be useful if you believe the attack or defence paramters given by the model is inacurate because you have some additional information that is not captured in the data.

Modifying the paramters can also be useful due to the same reasons as you might want to fit the model in two (or more) steps. For intance, if you look at the log-likelihood of the default model, and the model with the Rue-Salvesen adjustment, you will notice that they are almost identical. This is probably due to the model being nearly unidentifiable. That does not neccecarily mean that the Rue-Salvesen adjustment does not improve prediction, it might mean that it is hard to estimate the amount of adjustment based on the available data. In the paper by Rue & Salvesen they find that setting γ to 0.1 is a good choise. Here is how this can be done:

``` r
# Copy the default model.
gm_res_rs2 <- gm_res

# set the gamma parameter to 0.1
gm_res_rs2$parameters$gamma <- 0.1

# Make a few predictions to compare.
predict_result(gm_res, team1=to_predict1, team2=to_predict2, return_df = TRUE)
```

    ##               team1     team2        p1        pd        p2
    ## 1           Arsenal    Fulham 0.6195517 0.2036457 0.1768027
    ## 2 Manchester United   Chelsea 0.6735208 0.1844373 0.1420419
    ## 3  Bolton Wanderers Liverpool 0.2611863 0.2567973 0.4820164

``` r
predict_result(gm_res_rs2, team1=to_predict1, team2=to_predict2, return_df = TRUE)
```

    ##               team1     team2        p1        pd        p2
    ## 1           Arsenal    Fulham 0.6045881 0.2084048 0.1870071
    ## 2 Manchester United   Chelsea 0.6538287 0.1918135 0.1543579
    ## 3  Bolton Wanderers Liverpool 0.2780517 0.2603784 0.4615699

Other packages
==============

There are some other packages out there that has similar functionality as this package. The [regista](https://github.com/Torvaney/regista) package implements a more general interface to fit the Dixon-Coles model. The [fbRanks](https://cran.r-project.org/web/packages/fbRanks/index.html) package implements only the Dixon-Coles time weighting functionality, but has a lot of other features.

References
==========

-   Mark J. Dixon and Stuart G. Coles (1997) Modelling Association Football Scores and Inefficiencies in the Football Betting Market.
-   Håvard Rue and Øyvind Salvesen (2001) Prediction and Retrospective Analysis of Soccer Matches in a League.
