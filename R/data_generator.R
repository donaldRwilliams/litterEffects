#' Data Generator
#'
#' This function allows the user generate data with given characteristics.
#'@param b_0 intercept (e.g., mean of group one)
#'@param b_treat treatment effect (e.g., difference from b_0)
#'@param icc total variance explained by litter
#'@param v_overall total variance
#'@param n_litters number of litters
#'@param pups_litter observations per litter (must be even)
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
#'                    n_litters = n_litters, pups_litter = pups_litter

data_generator <-  function(b_0, b_treat, icc, v_overall, n_litters, pups_litter) {
  # simulate data from a random intercept model
  # Returns:
  #   a data.frame with the columns litter, treat, and y
  v_litter <- icc * v_overall
  v_error <- v_overall - v_litter
  litter <- rep(1:n_litters, each = pups_litter)
  # two treatments
  treat <- rep(0:1, each = pups_litter * n_litters / 2)
  treat <- factor(treat, labels = c('C', 'T'))
  # litter effect
  litter_eff <- rnorm(n_litters, 0, sqrt(v_litter))
  # residual
  residual <- rnorm(n_litters * pups_litter, 0, sqrt(v_error))
  # the outcome measure
  y <- b_0 + b_treat * (treat == 'T') + litter_eff[litter] + residual
  litter <- factor(paste0('l', litter))
  data.frame(litter, treat, y)
}