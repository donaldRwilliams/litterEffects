#' Retrospective Power for the the treatment effect
#'
#' This function computes power for observed values (e.g.,
#' the treatment effect, icc, overall variance, etc.)
#'@param nsims number of iterations. Should be > 1,000, but smaller values can build intution for expected power
#'@param outcome predicted observations
#'@param treatment (two groups: high vs low LG)
#'@param litter litter variable (see my_data for example)
#'@export
#'@examples



retropsective_power <- function(nsims, outcome, treatment, litter){
  dat_temp <- data.frame(litter, treatment)
  data_temp <- dat_temp %>% 
    group_by(litter) %>% 
    summarise(retro_func(treatment))
  pups_temp <- data_temp[,2]
  pups_litter <-  mean(pups_temp$`retro_func(treatment)`)
  n_litters <- length(unique(litter))
  mod <- lmerTest::lmer(outcome ~ treatment + (1|litter))
  var <- as.data.frame(lme4::VarCorr(mod,comp="Variance"))
  v_overall <- var$vcov[1] + var$vcov[2]
  icc <- var$vcov[1] / (var$vcov[1] + var$vcov[2])
  b_treat  <- summary(mod)$coefficients[[2]]
  b_0 <- summary(mod)$coefficients[[1]]
  #delta_t <- b_treat/sqrt(var$vcov[1] + var$vcov[2])
  m <- mean(replicate(nsims, 
                      retro_dat_fun(b_0 = b_0, b_treat = b_treat,
                                    icc = icc, n_litters = n_litters, 
                                    v_overall = v_overall,
                                    pups_litter = pups_litter)) < 0.05)
  list(retrospective_power = m)
}
