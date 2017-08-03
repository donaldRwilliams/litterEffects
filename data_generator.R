#' GEE Bias Correction testing 1, 2
#'
#' This function allows the user to determine the "correct" bias correction
#' for generalized estimating equations. The corrections are to ensure the
#' standard error of the estimates is not underestimated in small sample
#' situations. When uncorrected, GEE's can have error rates that exceed
#' nominal levels (alpha = 0.05).
#' data_generator()

data_gen <-  function(b_0, b_treat, icc, v_overall, n_litters, pups_litter) {
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