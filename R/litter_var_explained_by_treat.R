#' Between-litter variance explained by
#' the treatment effect
#'
#' This function computes the between_litter variance
#' explained by the treatment effect. Note that treatment
#' does not necessarily reflect and experimental design.
#' In this context, the treatment is a comparison between
#' two groups (e.g., high vs low lg).
#' 
#' Note that, in some instances, the variance explained
#' can be negative. In this case, there is more betweem-litter
#' variance after the inclusion of the treatment effect in the
#' model. This is cannot be interpreted directly, but does 
#' indicate that the treatment effect did not explain any
#' of the between-litter variance
#'@param outcome the observations to be predicted
#'@param treatment predictor (two groups: high vs low LG)
#'@export litter litter variable (see my_data for example)
#'@examples
#'
#'# specify values
#'b_0 <- 5
#'b_treat <- 2
#'icc <- 0.5
#'v_overall <- 10
#'n_litters <- 12
#'pups_litter <- 4

#'my_data <- data_generator(b_0 = b_0, b_treat = b_treat, 
#'                          icc = icc, v_overall = v_overall, 
#'                          n_litters = n_litters, 
#'                          pups_litter = pups_litter)
#'                    
#'                    
#'outcome <- my_data$y
#'treatment <- my_data$treatment
#'litter <- my_data$litter
#'var_explained_by_treat(outcome = outcome, 
#'                        treatment = treatment, 
#'                        litter = litter)


litter_var_explained_by_treat <- function(outcome, treatment, litter){
  mod_one <- lmerTest::lmer(outcome ~ 1 + (1|litter))
  mod_two <- lmerTest::lmer(outcome ~ treatment + (1|litter))
  one_var <- as.data.frame(lme4::VarCorr(mod_one),comp="Variance")
  two_var <- as.data.frame(lme4::VarCorr(mod_two),comp="Variance")
  list(litter_var_explained_by_treat = (one_var$vcov[1] - two_var$vcov[2])/one_var$vcov[1] )
}