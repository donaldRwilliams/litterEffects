bias_func <-  function(b_0, b_treat, icc, v_overall, 
                       n_litters, pups_litter) {
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
  my_data <- data.frame(litter, treat, y)
  capture.output(
    m_gee <-  suppressMessages(gee::gee(y ~ treat, id = litter, data = my_data, 
                                        family = gaussian, corstr = "exchangeable")))
  saws_gee <- saws::geeUOmega(m_gee)
  d1 <- saws::saws(saws_gee, method = "d1")[[9]][[2]]
  d2 <- saws::saws(saws_gee, method = "d2")[[9]][[2]]
  d3 <- saws::saws(saws_gee, method = "d3")[[9]][[2]]
  d4 <- saws::saws(saws_gee, method = "d4")[[9]][[2]]
  d5 <- saws::saws(saws_gee, method = "d5")[[9]][[2]]
  dm <- saws::saws(saws_gee, method = "dm")[[9]][[2]]
  c(d1, d2, d3, d4, d5, dm)
}
