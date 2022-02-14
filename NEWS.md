

# Version 0.6

 * New functions predict_btts() and pbtts() for computing Both-Teams-To-Score probabilities.
 * New function score_predictions(), for evaluating prediction accuracy using Ranked Probability Scores, Brier scores, and Log scores.
 * New function league_table() that computes a league table with points and useful summary statistics. 


# Version 0.5
 
 * New function expg_from_ou().
 * New function p1x2().
 * Bugfix: predict_goals() with return_df=TRUE did not work properly when the team arguments were factors.



# Version 0.4.3

  * Pseudo R-squared is now computed for fitted CMP models.
  * Can now do weighted estimation of the CMP dispersion parameter with the upsilon.ml() function.
  * New argument maxgoal in predict_goals()
  * predict_expg() does not return a data.frame with row.names if return_df=TRUE.


# Version 0.4.2

  * New function dDCP(), the probability mass function for the Dixon-Coles model.


# goalmodel 0.4.1

  * New function qCMP(), the quantile function for the Conway-Maxwell-Poisson distribution.
  
  * The predict_goals() function has a new argument return_df. If return_df = TRUE, the function returns a long data.frame instead of a list of matrices. 

# goalmodel 0.4

 * The prediction functions now works when trying to predict the outcome when a team is not in the model. The predictions will be NA.

  * Some of the functions related to the Conway-Maxwell-Poisson distribution is re-implemented in Rcpp. This should give somewhat faster estimation.

  * Bugfix: The returned loglikelihood (and R squared, aic, etc) when using weights were sometimes wrong. This is now fixed.



# goalmodel 0.3

  * New function expg_from_probabilities(). This converts win-draw-lose probabilities to expected goals.

  * Two new functions matches_last_xdays() and days_since_last_match(). These are useful if you want to include information about the effect of having a busy schedule into your model.

  * Some additional checks have been added make sure that the network of all team that has played each other is fully connected.



# goalmodel 0.2

   * New faster model fitting. Some models use the built-in glm.fit function to estimate the attack and defence parameters. If the model can not be estimated with the glm.fit function, the results from glm.fit is used as starting values for subsequent fitting. All this should give faster estimation.

   * Can now fit Conway-Maxwell-Poisson (CMP) models. This is still a bit slow and unstable, so two-step estimation is recomended. See the section on the CMP below for more info. Several new functions related to the CMP model is also available if you want to use them for additional modelling.

   * The model = 'ls' option is removed. The option to use model = 'gaussian', is still available and should be equivalent to the old 'ls' option.

   * Some changes to what is returned by the goalmodel function, but this should not cause much problems to your existing code since it is only related to some of the more technical parts of the output.

# goalmodel 0.1
   * Initial release

