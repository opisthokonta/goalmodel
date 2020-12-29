
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
    - Initial release

