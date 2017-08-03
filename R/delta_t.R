#' Effect Size: Delta Total Variance
#'
#' This function computes an effect size for the treatment
#' effect. The paper defines delta t in formula X
#' 
#' Note that, in some instances, the variance explained
#' can be negative. In this case, there is more betweem-litter
#' variance after the inclusion of the treatment effect in the
#' model. This is cannot be interpreted directly, but does 
#' indicate that the treatment effect did not explain any
#' of the between-litter variance
#'@param formula lme4 arugments y ~ x + (1|litter)
#'@param data 
#'@export
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
#'                          n_litters = n_litters, pups_litter = pups_litter)
#'
#'
#'delta_t(y ~ treat + (1|litter), data = my_data)

delta_t <- function(formula, data){
mod <- lmerTest::lmer(formula, data = data)
est <- summary(mod)$coefficients[[2]]
var <- as.data.frame(lme4::VarCorr(mod,comp="Variance"))
delta_t <- est / sqrt(var$vcov[1] + var$vcov[2])
list(delta_t = delta_t)
}