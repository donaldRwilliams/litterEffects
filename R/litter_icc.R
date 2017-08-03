#' ICC for litter
#'
#' This function computes ICC for litter. This is the residual 
#' variance explained by litter
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
#'                    icc = icc, v_overall = v_overall, 
#'                    n_litters = n_litters, pups_litter = pups_litter)
#'                    
#'litter_icc(y ~ treat + (1|litter), data = my_data)


litter_icc <- function(formula, data){
  mod <- lmerTest::lmer(formula, data)
  var <- as.data.frame(lme4::VarCorr(mod,comp="Variance"))
  icc <- var$vcov[1] / (var$vcov[1] + var$vcov[2])
  list(icc = icc)
}