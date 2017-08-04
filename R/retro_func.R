#' Called by Retrospective Power
#' @export
#' @examples

retro_func <- function(x){
  length(x)
}

retro_pow_fun <- function (b_0 = b_0, b_treat = b_treat, icc, v_overall, 
                           n_litters, pups_litter) {
  dat <- litterEffects::data_generator(b_0 = b_0, b_treat = b_treat, 
                                       v_overall = v_overall, icc = icc, 
                                       n_litters = n_litters, 
                                       pups_litter = pups_litter)
  m_lmer <- lmerTest::lmer(y ~ treat + (1 | litter), data = dat)
  lmerTest::anova(m_lmer)[1, 6]
}